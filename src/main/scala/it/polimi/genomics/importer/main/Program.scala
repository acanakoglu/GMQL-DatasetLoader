package it.polimi.genomics.importer.main

import java.io.File
import java.nio.charset.Charset
import java.util

import it.polimi.genomics.core.GDMSUserClass
import it.polimi.genomics.repository.FSRepository.DFSRepository
import it.polimi.genomics.repository.GMQLSample
import org.slf4j._

import scala.collection.JavaConverters._
import scala.xml.{Elem, NodeSeq, XML}


object Program {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)
  logger.info(getClass.getResource("/gmql_conf/").getFile)


  it.polimi.genomics.repository.Utilities.confFolder = new File("./gmql_conf/").getAbsolutePath//getClass.getResource("/gmql_conf/").getFile
  it.polimi.genomics.repository.Utilities()
  val repo = new DFSRepository


  var regionRootFolder: String = _
  var repositoryPublicDataFolder: String = _
  var username: String = _

  def main(args: Array[String]): Unit = {
    logger.info("Charset.defaultCharset:" + Charset.defaultCharset)
    //    new RepositoryManagerV2("public").unregisterUser("public")
    //    new RepositoryManagerV2("public").registerUser("public")

    val configFile = new File(args.head)
    if (!configFile.exists()) {
      logger.error("file not exists: " + configFile.getAbsolutePath)
      return
    }

    logger.debug("File exists")
    val xmlConfigFile: Elem = XML.loadFile(configFile)



    regionRootFolder = (xmlConfigFile \ "region_root_folder").text
    logger.info("regionRootFolder: " + regionRootFolder)
    repositoryPublicDataFolder = (xmlConfigFile \ "repository_public_data_folder").text
    logger.info("repositoryDataFolder: " + repositoryPublicDataFolder)
    username = (xmlConfigFile \ "username").text
    logger.info("username: " + username)

    val datasetToDeleteNames = xmlConfigFile \ "delete_datasets" \ "dataset_name"

    val datasetsXml = xmlConfigFile \ "datasets_xml" \ "dataset"
    val datasetsFolder = xmlConfigFile \ "datasets_folder" \ "dataset"

    datasetToDeleteNames.foreach(datasetName => deleteDataset(datasetName.text))
    loadFromDatasetXml(datasetsXml)
    loadFromDirectory(datasetsFolder)


  }

  def loadFromDirectory(datasets: NodeSeq): Unit = {
    for (a <- datasets) {
      val datasetName = (a \ "dataset_name").text
      logger.info("datasetName: " + datasetName)
      val directory = (a \ "directory").text
      logger.info("directory: " + directory)
      val schema = (a \ "schema").text
      logger.info("schema: " + schema)
      if (!new File(schema).exists()) {
        logger.error("schema file doesn't exists: " + schema)
        return
      }
      if (!new File(directory).exists()) {
        logger.error("directory doesn't exists: " + directory)
        return
      }

      val samples = getSampleFromDirectory(directory)
      if (samples != null)
        addDataset(datasetName, schema, samples)
    }
  }


  def getSampleFromDirectory(directory: String) = {
    getListOfFiles(directory)
      .filter(_.getName.endsWith(".bed") || true)
      .filter(f => new File(f.getAbsolutePath + ".meta").exists)
      .map { a =>
        val regionFile = a.getAbsolutePath
        logger.debug("regionFile: " + regionFile)
        val metaFile = regionFile + ".meta"
        logger.debug("metaFile:   " + metaFile)
        GMQLSample(
          regionFile,
          metaFile,
          null)
      }.asJava
  }

  def loadFromDatasetXml(datasets: NodeSeq): Unit = {
    for (dataset <- datasets) {
      val datasetName = (dataset \ "dataset_name").text
      val oldDatasetName = Option((dataset \ "old_dataset_name").text).filter(_.trim.nonEmpty).getOrElse(datasetName)

      logger.info(s"datasetName: $datasetName, oldDatasetName: $oldDatasetName")


      val datasetFile = repositoryPublicDataFolder + File.separator + "datasets" + File.separator + oldDatasetName + ".xml"
      if (!new File(datasetFile).exists()) {
        logger.error("Schema file doesn't exists: " + datasetFile)
        return
      }

      val schemaFile = repositoryPublicDataFolder + File.separator + "schema" + File.separator + oldDatasetName + ".schema"
      if (!new File(schemaFile).exists()) {
        logger.error("Schema file doesn't exists: " + schemaFile)
        return
      }

      val samples = getSampleFromDatasetXml(regionRootFolder, datasetFile)
      if (samples != null)
        addDataset(datasetName, schemaFile, samples)
    }
  }


  def getSampleFromDatasetXml(regionRootFolder: String, datasetFile: String): util.List[GMQLSample] = {
    (XML.loadFile(datasetFile) \\ "url").map { x =>
      val regionFile = regionRootFolder + x.text
      logger.debug("regionFile: " + regionFile)
      val metaFile = regionFile + ".meta"
      logger.debug("metaFile:   " + metaFile)
      if (!new File(regionFile).exists()) {
        logger.error("Region file doesn't exists: " + regionFile)
        return null
      }
      if (!new File(metaFile).exists()) {
        logger.error("Meta file doesn't exists: " + metaFile)
        return null
      }
      GMQLSample(
        regionFile,
        metaFile,
        null)
    }.asJava
  }

  def addDataset(datasetName: String, schemaFile: String, samples: util.List[GMQLSample]) = {
    if (samples.size() > 0) {
      logger.debug("Trying to add " + datasetName + " to user: " + username)

      //      deleteDataset(datasetName)

      if (!existDataset(datasetName))
        try {
          repo.importDs(
            datasetName,
            username,
            GDMSUserClass.PUBLIC,
            samples,
            schemaFile)
          logger.info("import for dataset " + datasetName + " completed")
        }
        catch {
          case e: Throwable => logger.error("import failed: " + e.getMessage)
        }
      else
        logger.warn("Already exists, skipped " + datasetName)

    }
  }

  def deleteDataset(datasetName: String) {
    if (existDataset(datasetName)) {
      try {
        repo.deleteDS(datasetName, username)
        logger.info(s"DeleteDS was called for dataset: $datasetName")
      }
      catch {
        case e: Throwable => logger.error("Dataset " + datasetName + " cannot be deleted!!")
      }
    } else
      logger.warn(s"deleteDataset: Dataset($datasetName) doesn't exists")
  }

  def existDataset(datasetName: String) = (false /: repo.listAllDSs(username).asScala.map(_.position)) (_ || _.equals(datasetName))



  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }
}

