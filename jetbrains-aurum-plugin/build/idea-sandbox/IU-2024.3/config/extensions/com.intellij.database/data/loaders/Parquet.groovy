// IJ: extensions = parquet displayName = Parquet
package extensions.data.loaders

@Grab(group='org.apache.parquet', module='parquet-column', version='1.12.3')
@Grab(group='org.apache.parquet', module='parquet-hadoop', version='1.12.3')
@Grab(group='org.apache.hadoop', module='hadoop-common', version='3.3.1')
@Grab(group='com.google.code.gson', module='gson', version='2.11.0')

import org.apache.parquet.hadoop.ParquetFileReader
import org.apache.parquet.column.page.PageReadStore
import org.apache.parquet.io.api.*
import org.apache.parquet.io.RecordReader
import org.apache.parquet.schema.*
import org.apache.hadoop.fs.Path
import org.apache.hadoop.conf.Configuration
import org.apache.parquet.hadoop.util.HadoopInputFile
import org.apache.parquet.io.ColumnIOFactory
import com.google.gson.*

def classLoader = getClass().getClassLoader()
Thread.currentThread().setContextClassLoader(classLoader)

LOADER.load { ctx ->
  Thread.currentThread().setContextClassLoader(classLoader)
  loadParquet(ctx.getParameters()["FILE"], ctx.getDataConsumer())
}

def loadParquet(path, dataConsumer) {
  ParquetFileReader reader = ParquetFileReader.open(HadoopInputFile.fromPath(new Path(path), new Configuration()))
  MessageType schema = reader.getFooter().getFileMetaData().getSchema()
  //System.out.println(schema)

  def materializer = new RowMaterializer(schema, dataConsumer)
  materializer.describeColumns(dataConsumer)
  try {
    for (PageReadStore pages : reader.readNextRowGroup()) {
      long rows = pages.getRowCount()
      def columnIO = new ColumnIOFactory().getColumnIO(schema)

      RecordReader recordReader = columnIO.getRecordReader(pages, materializer)

      for (int i = 0; i < rows; i++) {
        recordReader.read()
      }
    }
  }
  finally {
    reader.close()
  }
}

class RowMaterializer extends RecordMaterializer<Void> {

  private RowGroupConverter root
  def dataConsumer

  RowMaterializer(MessageType schema, dataConsumer) {
    this.dataConsumer = dataConsumer
    this.root = new RowGroupConverter(null, 0, schema, true) {
      void flushParent() {
        if (current == null) return
        dataConsumer.consume(getCurrentRecord())
        current = null
      }
    }
  }

  void describeColumns(dataConsumer) {
    def names = new String[root.size]
    def types = new Class[root.size]
    root.describeColumns(names, types, 0)
    dataConsumer.consumeColumns(names, types)
  }

  Void getCurrentRecord() {
    return null
  }

  GroupConverter getRootConverter() {
    return root
  }
}

class RowPrimitiveConverter extends PrimitiveConverter {
  private final RowGroupConverter parent
  private final int index
  private final PrimitiveType schema

  RowPrimitiveConverter(RowGroupConverter parent, int index, PrimitiveType schema) {
    this.parent = parent
    this.index = index
    this.schema = schema
  }

  void addValue(Object value) {
    this.parent.addValue(value, index, isList())
  }

  private boolean isList() {
    return schema.getRepetition() == Type.Repetition.REPEATED
  }

  void addBinary(Binary value) {
    addValue(value.getBytes())
  }

  void addBoolean(boolean value) {
    addValue(value)
  }

  void addDouble(double value) {
    addValue(value)
  }

  void addFloat(float value) {
    addValue(value)
  }

  void addInt(int value) {
    addValue(value)
  }

  void addLong(long value) {
    addValue(value)
  }

  def describeColumns(String[] names, Class[] classes, int index) {
    def realIdx = index + this.index
    names[realIdx] = schema.getName()
    def clazz = isList() ? List.class : schema.getPrimitiveTypeName().javaType
    classes[realIdx] = clazz == Binary.class ? byte[].class : clazz
  }
}


class JSONRowGroupConverter extends GroupConverter {
  JSONRowGroupConverter parent
  GroupType schema
  JsonObject container
  String key
  Converter[] converters

  JSONRowGroupConverter(JSONRowGroupConverter parent, String key, GroupType schema) {
    this.parent = parent
    this.schema = schema
    this.key = key
    converters = new Converter[schema.getFieldCount()]

    for (int i = 0; i < converters.length; i++) {
      def type = schema.getType(i)
      if (type.isPrimitive()) {
        def pParent = this
        converters[i] = new PrimitiveConverter() {
          void addValue(JsonElement value) {
            addValue(pParent, type.getName(), type, value)
          }

          void addBinary(Binary value) {
            def array = new JsonArray()
            value.getBytes().each { array.add(it) }
            addValue(array)
          }

          void addBoolean(boolean value) {
            addValue(new JsonPrimitive(value))
          }

          void addDouble(double value) {
            addValue(new JsonPrimitive(value))
          }

          void addFloat(float value) {
            addValue(new JsonPrimitive(value))
          }

          void addInt(int value) {
            addValue(new JsonPrimitive(value))
          }

          void addLong(long value) {
            addValue(new JsonPrimitive(value))
          }
        }
      }
      else {
        converters[i] = new JSONRowGroupConverter(this, type.getName(), type)
      }
    }
  }

  void start() {
    container = new JsonObject()
  }

  Converter getConverter(int fieldIndex) {
    return converters[fieldIndex]
  }

  void end() {
    addValue(parent, key, schema, container)
  }

  void addValue(JSONRowGroupConverter parent, String key, Type schema, JsonElement value) {
    if (parent == null) return

    if (schema.getRepetition() != Type.Repetition.REPEATED) {
      parent.container.add(key, value)
      return
    }

    def list = parent.container.get(key)
    if (list == null) {
      list = new JsonArray()
      parent.container.add(key list)
    }
    list.add(value)
  }
}

class PlainRowGroupConverter extends GroupConverter {
  RowGroupConverter parent
  JSONRowGroupConverter delegate
  int index

  PlainRowGroupConverter(RowGroupConverter parent, int index, GroupType schema) {
    this.parent = parent
    this.delegate = new JSONRowGroupConverter(null, "", schema)
    this.index = index
  }

  void start() {
    delegate.start()
  }

  Converter getConverter(int fieldIndex) {
    return delegate.getConverter(fieldIndex)
  }

  void end() {
    delegate.end()
    parent.addValue(getCurrentRecord().toString(), index, true)
  }

  String getCurrentRecord() {
    return delegate.container
  }

  def describeColumns(String[] names, Class[] classes, int index) {
    def realIdx = index + this.index
    names[realIdx] = delegate.schema.getName()
    classes[realIdx] = String.class
  }
}

class RowGroupConverter extends GroupConverter {
  RowGroupConverter parent
  int index
  int size
  Object[] current
  Converter[] converters
  GroupType schema
  boolean flushable

  RowGroupConverter(RowGroupConverter parent, int index, GroupType schema, boolean flushable) {
    this.parent = parent
    this.index = index
    this.schema = schema

    converters = new Converter[schema.getFieldCount()]

    int size = 0
    this.flushable = flushable
    def subFlushable = flushable && converters.length == 1 && (
      schema instanceof MessageType || schema.getRepetition() != Type.Repetition.REPEATED)
    for (int i = 0; i < converters.length; i++) {
      def type = schema.getType(i)
      def isList = type.getRepetition() == Type.Repetition.REPEATED
      if (type.isPrimitive()) {
        converters[i] = new RowPrimitiveConverter(this, size, type)
        size += 1
      }
      else if (isList && !subFlushable) {
        converters[i] = new PlainRowGroupConverter(this, size, type)
        size += 1
      }
      else {
        converters[i] = new RowGroupConverter(this, size, type, subFlushable)
        size += converters[i].size
      }
    }
    this.size = size
  }

  void flushParent() {
    if (this.parent != null) this.parent.flush()
  }

  void flush() {
    if (flushable) {
      flushParent()
      current = null
    }
  }

  void start() {
    current = null
    getCurrentRecord() //materialize
  }

  Converter getConverter(int fieldIndex) {
    return converters[fieldIndex]
  }

  void end() {
    flush()
  }

  void addValue(Object value, int index, boolean repeat) {
    def realIdx = this.index + index
    def current = getCurrentRecord()
    if (repeat && !flushable) {
      def list = current[realIdx]
      if (list == null) {
        list = new ArrayList()
        current[realIdx] = list
      }
      list.add(value)
    }
    else {
      current[realIdx] = value
    }
    if (repeat) {
      flush()
    }
  }

  Object[] getCurrentRecord() {
    if (current != null) return current
    if (this.parent != null) return this.parent.getCurrentRecord()
    current = new Object[size]
    return current
  }

  def describeColumns(String[] names, Class[] classes, int index) {
    def realIdx = index + this.index
    converters.each {
      it.describeColumns(names, classes, realIdx)
    }
    if (shouldAddName()) {
      for (int i = 0; i < size; ++i) {
        names[realIdx + i] = schema.getName() + "." + names[realIdx + i]
      }
    }
  }

  private boolean shouldAddName() {
    if (schema instanceof MessageType) return false
    if (this.parent == null) return true
    return size != this.parent.size
  }
}
