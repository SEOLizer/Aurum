// IJ: extensions = xls;xlsx displayName = XLS
package extensions.data.loaders

@Grab('org.apache.poi:poi:5.2.5')
@Grab('org.apache.poi:poi-ooxml:5.2.5')
import org.apache.poi.ss.usermodel.CellType
import org.apache.poi.ss.usermodel.WorkbookFactory

LOADER.load { ctx ->
  loadXls(ctx.getParameters()["FILE"], ctx.getDataConsumer())
}

def loadXls(path, dataConsumer) {
  def wb = WorkbookFactory.create(new File(path))
  def evaluator = wb.getCreationHelper().createFormulaEvaluator();

  def sheet = wb.getSheetAt(0)

  produceSheet(sheet, evaluator, dataConsumer)
}

private void produceSheet(sheet, evaluator, dataConsumer) {
  def idx = 0
  sheet.forEach { row ->
    def res = extractRow(row, evaluator)
    if (!res.isEmpty()) {
      def cur = row.getRowNum()
      while (idx < cur - 1) {
        dataConsumer.consume(new Object[0])
        ++idx
      }
      idx = cur
      dataConsumer.consume(res.toArray())
    }
  }
}

private List<Object> extractRow(row, evaluator) {
  def res = new ArrayList<Object>()
  row.forEach { cell ->
    def cur = cell.getColumnIndex()
    while (res.size() < cur) {
      res.add(null)
    }
    def v = evaluator.evaluate(cell)
    def rv = cellVal(v)
    res.add(rv)
  }
  res
}

def cellVal(cell) {
  if (cell == null) return null
  switch (cell.getCellType()) {
    case CellType.BOOLEAN:
      return cell.getBooleanValue()
    case CellType.STRING:
      return cell.getStringValue()
    case CellType.NUMERIC:
      return cell.getNumberValue()
    case CellType.BLANK:
      return null
    default:
      return cell.formatAsString()
  }
}
