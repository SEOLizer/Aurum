// IJ: extensions = shp displayName = SHP
package extensions.data.loaders

@GrabResolver(name='OSGeo', root='https://repo.osgeo.org/repository/geotools-releases/')
@Grab('org.geotools:gt-shapefile:31.0')
@Grab('org.geotools:gt-epsg-hsql:31.0')
import org.geotools.api.data.FileDataStoreFinder
import org.geotools.api.feature.type.GeometryType
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS
import org.locationtech.jts.geom.Geometry

def classLoader = getClass().getClassLoader()
Thread.currentThread().setContextClassLoader(classLoader)
outCRS = CRS.decode("EPSG:4326", true)

LOADER.load { ctx ->
  Thread.currentThread().setContextClassLoader(classLoader)
  loadShp(ctx.getParameters()["FILE"], ctx.getDataConsumer())
}

def loadShp(path, dataConsumer) {
  def store = FileDataStoreFinder.getDataStore(new File(path));
  if (store == null) return
  try {
    def types = store.getTypeNames()
    def type = types[0]
    def featureSource = store.getFeatureSource(type)
    produceFeatureSource(featureSource, dataConsumer)
  }
  finally {
    store.dispose()
  }
}

void produceFeatureSource(featureSource, dataConsumer) {
  def schema = featureSource.getSchema()
  def attrs = schema.getAttributeDescriptors()
  dataConsumer.consumeColumns(
    attrs.collect { it.getLocalName() }.toArray(new String[0]),
    attrs.collect { it.getType().getBinding() }.toArray(new Class[0])
  )
  def accessors = attrs.collect { attr ->
    createAccessor(attr)
  }
  def features = featureSource.getFeatures()
  try (def fIt = features.features()) {
    while (fIt.hasNext()) {
      def feature = fIt.next()
      dataConsumer.consume(accessors.collect { it(feature) }.toArray())
    }
  }
}

def createAccessor(attr) {
  def defaultAccessor = { feature ->
    feature.getProperty(attr.getName()).getValue()
  }
  if (attr.getType() instanceof GeometryType && outCRS != null) {
    def dataCRS = attr.getType().getCoordinateReferenceSystem()
    def transform = CRS.findMathTransform(dataCRS, outCRS, true);
    return { feature ->
      def data = defaultAccessor(feature)
      if (data instanceof Geometry) {
        JTS.transform(defaultAccessor(feature), transform)
      }
      else {
        data
      }
    }
  }
  else {
    return defaultAccessor
  }
}
