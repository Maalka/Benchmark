package services

import java.io.{File, FileInputStream, InputStream}
import com.typesafe.scalalogging.LazyLogging

import javax.inject.{Inject, Singleton}
import org.jclouds.ContextBuilder
import org.jclouds.blobstore.{BlobStore, BlobStoreContext}
import org.jclouds.filesystem.reference.FilesystemConstants
import play.api.Configuration

@Singleton
class StorageService @Inject ()(
                                 configuration: Configuration
                               ) extends LazyLogging {

  val properties = new java.util.Properties()

  val storagePath: String = configuration.getOptional[String]("jclouds.filesystem.path").getOrElse("/tmp")

  properties.setProperty(FilesystemConstants.PROPERTY_BASEDIR, storagePath)

  def getBlobStore: BlobStore = {
    ContextBuilder.newBuilder("filesystem")
      .overrides(properties)
      .buildView(classOf[BlobStoreContext])
      .getBlobStore
  }

  def deleteFile(container: String, name: String): Unit = {
    val blobStore = getBlobStore
    blobStore.removeBlob(container, name)
  }

  def putFileFromFile(container: String, name: String, file: File): Unit = {
    val blobStore = getBlobStore
    blobStore.createContainerInLocation(null, container)
    val fileInputStream = new FileInputStream(file)
    val blob = blobStore.blobBuilder(name).payload(fileInputStream).build()
    blobStore.putBlob(container, blob)
    fileInputStream.close()
  }

  def putFile(container: String, name: String, data: Array[Byte]): String = {
    val blobStore = getBlobStore
    blobStore.createContainerInLocation(null, container)
    val blob = blobStore.blobBuilder(name).payload(data).build()
    blobStore.putBlob(container, blob)
  }

  def getFile(container: String, name: String): InputStream = {
    val blobStore = getBlobStore
    val blob = if (blobStore.blobExists(container, name)) {
      blobStore.getBlob(container, name)
    } else {
      throw new Exception("File Not Found")
    }
    val payload = blob.getPayload
    payload.openStream
  }
}
