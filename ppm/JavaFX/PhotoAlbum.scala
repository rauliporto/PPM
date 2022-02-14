package JavaFX

import JavaFX.Album.Images
import javafx.application.Application
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

import java.io.{BufferedWriter, File, FileNotFoundException, FileWriter}
import java.util.Scanner

class PhotoAlbum extends Application {
  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("Photo Album")
    val fxmlLoader =
      new FXMLLoader(getClass.getResource("controller.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    val controller: Controller = fxmlLoader.getController
    if (!FxApp.album.imgs.isEmpty) {
      controller.setImage(FxApp.album.imgs.head._2)
      controller.setLabelName((FxApp.album.imgs.head._2.drop(7).dropRight(4)))
    }
    val scene = new Scene(mainViewRoot)

    primaryStage.setResizable(false)
    primaryStage.setScene(scene)
    primaryStage.show()

  }
}

object FxApp {
  var path: String = "ppm/images/photoAlbum.txt"
  var album = loadAlbum(path)

  println(path)


  def writeFile(alb: Album): Unit = {
    def aux(lstimg: Images): Unit = {
      val file = new File(path)
      val bw = new BufferedWriter(new FileWriter(file))
      for (line <- (lstimg.map(x => x._2))) {
        bw.write(line + "\n")
      }
      bw.close()
    }

    aux(alb.imgs)
  }

  def readFile(str: String): List[String] = {
    var album = List[String]()
    println(str)

    val file = new File(str)
    try {
      val scanner = new Scanner(file)
      try {
        while (scanner.hasNextLine) {
          val line = scanner.nextLine
          album = album :+ line
        }
        scanner.close()
      } catch {
        case e: FileNotFoundException =>
          System.err.println("Ficheiro não encontrado!!!")
          System.exit(1)
      } finally if (scanner != null) scanner.close()
    }
    album
  }

  def loadAlbum(pathAlbum: String): Album = {
    val album = new Album(Nil)
    val list = readFile(pathAlbum)

    def aux(l: List[String], alb: Album): Album = {
      l match {
        case Nil => alb
        case x :: xs => {
          val alb1: Album = alb.addImage(x)
          aux(xs, alb1)
        }
      }
    }

    aux(list.reverse, album)
  }

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[PhotoAlbum], args: _*)
  }
}