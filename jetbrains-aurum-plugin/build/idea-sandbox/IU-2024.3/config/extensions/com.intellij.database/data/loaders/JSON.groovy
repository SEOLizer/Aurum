// IJ: extensions = json displayName = JSON tableFirstFormat=false
package extensions.data.loaders

@Grab("com.fasterxml.jackson.core:jackson-core:2.16.1")
@Grab("com.fasterxml.jackson.core:jackson-databind:2.16.1")
import com.fasterxml.jackson.core.*
import com.fasterxml.jackson.databind.ObjectMapper

LOADER.load { ctx ->
  loadJson(ctx.getParameters()["FILE"], ctx.getDataConsumer())
}

def loadJson(path, dataConsumer) {
  new ObjectMapper().createParser(new File(path)).withCloseable { reader ->
    def tok = reader.nextToken()
    if (tok == null) return
    switch(tok) {
      case JsonToken.START_OBJECT:
        processMapAsRows(reader, dataConsumer)
        break
      case JsonToken.START_ARRAY:
        processArrayAsRows(reader, dataConsumer)
        break
      default:
        processValueAsRows(reader, dataConsumer)
        break
    }
  }
}

def nextAnyValue(reader) {
  reader.nextToken()
  return readAnyValue(reader)
}

def readAnyValue(reader) {
  def tok = reader.currentToken()
  switch (tok) {
    case JsonToken.START_OBJECT:
    case JsonToken.START_ARRAY:
      return reader.readValueAsTree()
    default:
      return reader.getText()
  }
}

def processMapAsRows(reader, dataConsumer) {
  while (true) {
    def name = reader.nextFieldName()
    if (name == null) break
    def value = nextAnyValue(reader)
    dataConsumer.consume(name, value)
  }
}

def processArrayAsRows(reader, dataConsumer) {
  def tok = reader.nextToken()
  switch(tok) {
    case JsonToken.START_OBJECT:
      processArrayOfMapsAsRows(reader, dataConsumer)
      break
    case JsonToken.START_ARRAY:
      processArrayOfArraysAsRows(reader, dataConsumer)
      break
    default:
      processArrayOfValuesAsRows(reader, dataConsumer)
      break
  }
}

def processArrayOfXAsRows(reader, dataConsumer, X) {
  while (true) {
    def tok = reader.currentToken()
    if (tok == null || tok == JsonToken.END_ARRAY) {
      return
    }
    dataConsumer.consume(X())
    reader.nextToken()
  }
}

def processArrayOfArraysAsRows(reader, dataConsumer) {
  processArrayOfXAsRows(reader, dataConsumer) {
    extractArrayRow(reader).toArray()
  }
}

def processArrayOfValuesAsRows(reader, dataConsumer) {
  processArrayOfXAsRows(reader, dataConsumer) {
    new Object[]{ readAnyValue(reader) }
  }
}

def processArrayOfMapsAsRows(reader, dataConsumer) {
  def colIdx = new HashMap<String, Integer>()
  def processor = { extractMapRow(reader, colIdx).toArray() }
  def row = processor()
  def names = new ArrayList()
  def types = new ArrayList()
  colIdx.forEach { key, value ->
    addValue(names, key, value)
    addValue(types, row[value]?.getClass(), value)
  }
  dataConsumer.consumeColumns(names.toArray(new String[0]), types.toArray(new Class[0]))
  dataConsumer.consume(row)
  reader.nextToken()
  processArrayOfXAsRows(reader, dataConsumer, processor)
}


def processValueAsRows(reader, dataConsumer) {
  def value = nextAnyValue(reader)
  dataConsumer.consume(name, value)
}

def extractArrayRow(reader) {
  def res = new ArrayList()
  if (reader.currentToken() != JsonToken.START_ARRAY) {
    res.add(readAnyValue(reader))
    return res
  }
  while (true) {
    def tok = reader.nextToken()
    if (tok == null || tok == JsonToken.END_ARRAY) {
      return res
    }
    res.add(readAnyValue(reader))
  }
}

def extractMapRow(reader, Map<String, Integer> colIdx) {
  def res = new ArrayList(colIdx.size())
  if (reader.currentToken() != JsonToken.START_OBJECT) {
    res.add(readAnyValue(reader))
    return res
  }
  while (true) {
    def tok = reader.nextToken()
    if (tok == null || tok == JsonToken.END_OBJECT) {
      return res
    }
    def name = reader.getCurrentName()
    def value = nextAnyValue(reader)
    addValue(res, value, getOrAllocateIdx(name, colIdx))
  }
}

int getOrAllocateIdx(String name, Map<String, Integer> colIdx) {
    def idx = colIdx[name]
    if (idx == null) {
      idx = colIdx.size()
      colIdx[name] = idx
    }
    return idx
}

def addValue(List res, value, int idx) {
    while (res.size() < idx) res.add(null)
    res[idx] = value
}