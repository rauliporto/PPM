package qtree

import JavaFX.Album
import JavaFX.Album._
import qtree.Coords.Coords
import qtree.QuadTree._
import qtree.TextUserInterface._

import java.io.{File, FileNotFoundException}
import java.util.Scanner

case class TextUserInterface() {
  def interface():Unit = TextUserInterface.interface(loadAlbum())
}

object TextUserInterface {


  var imgSelec:Image = (0, "")
  val path :String = "ppm/"
  var pathImage: String = path + imgSelec._2

  // ---------------------------------------------------------------------------
  // -----------------------------  Aux  ---------------------------------------
  // ---------------------------------------------------------------------------

  def editImage(f:QTree[Coords] => QTree[Coords]): Unit = {
    var change = convertToImage(f(convertToQTree()))
  }

  def convertToQTree(): QTree[Coords] ={
    println(pathImage)
    val a1 = convertImgToArray(pathImage)
    val a2 = convertBitMapToList(a1)
    val a3 = makeQTree(a2)
    a3
  }

  def convertToImage(qt: QTree[Coords]): Unit = {
    val b2 = makeListList(qt)
    val b1 = convertListToBitmap(b2)
    val b3 = convertArraytoImg(b1,pathImage)
  }

  def option():Unit = {
    println("Selecione um efeito:\n" + "1. Rodar para a direita\n" + "2. Rodar para a esquerda\n" + "3. Espelhamento Vertical\n" + "4. Espelhamento Horizontal\n"
      + "5. Efeito Sepia\n" + "6. Efeito Noise\n" + "7. Efeito Contraste\n" + "8. Scale\n" + "0. Sair" )
  }

  // ---------------------------------------------------------------------------
  // -----------------------------  Interface  ---------------------------------
  // ---------------------------------------------------------------------------

  def interface(alb:Album):Unit = {
    println("-")
    println(alb)
    println("-")
    println("Selecione o ID da imagem do Album:" )
    val scanner = new Scanner(System.in)
    var imgID = scanner.nextInt()
    while (imgID > alb.imgs.length || imgID < 1) {
      println("Insira um valor no intervalo[" + 1 + ", " + alb.imgs.length + "]")
      imgID = scanner.nextInt()
    }
    imgSelec = alb.imgs(imgID-1)
    pathImage= path + imgSelec._2
    //println(imgSelec)
    println(imgSelec)
    println(pathImage)
    println("Imagem " + imgSelec._2 + " selecionada.")
    println("-")
    option()
    var selec = scanner.nextInt()
    while(selec != 0) {
      selec match {
        case 1 =>
          editImage(rotateR)
          println("Imagem rodada para a direita com sucesso!!!")
          option()
          selec = scanner.nextInt()

        case 2 =>
          editImage(rotateL)
          println("Imagem rodada para esquerda com sucesso!!!")
          option()
          selec = scanner.nextInt()

        case 3 =>
          editImage(mirrorV)
          println("Imagem espelhada verticalmente com sucesso!!!")
          option()
          selec = scanner.nextInt()

        case 4 =>
          editImage(mirrorH)

          println("Imagem espelhada horizontalmente com sucesso!!!")
          option()
          selec = scanner.nextInt()

        case 5 =>
          convertToQTree()
          val change = convertToImage(mapColourEffect(sepiaEffect,convertToQTree()))

          println("Efeito sepia aplicado com sucesso!!!")
          option()
          selec = scanner.nextInt()

        case 6 =>
          convertToQTree()
          val change = convertToImage(mapColourEffect(noiseEffect,convertToQTree()))

          println("Efeito noise aplicado com sucesso!!!")
          option()
          selec = scanner.nextInt()

        case 7 =>
          convertToQTree()
          val change = convertToImage(mapColourEffect(contrastEffect,convertToQTree()))

          println("Efeito noise aplicado com sucesso!!!")
          option()

          selec = scanner.nextInt()

        case 8 =>
          println("Insira um fator de escala [>1]: ")
          selec = scanner.nextInt()
          convertToQTree()
          val change = convertToImage(scale(selec,convertToQTree()))

          println("Efeito scale aplicado com sucesso!!!")
          option()

          selec = scanner.nextInt()

        case _ => println("Insira um valor no intervalo de [" + 1 + ", " + 8 + "]")
          selec = scanner.nextInt()

      }
    }
    scanner.close()
    println("A Sair!!!")
  }

  def readFile(str: String): List[String] = {
    var album = List[String]()
    val file = new File(str)
    try {
      val scanner = new Scanner(file)
      try {
        while (scanner.hasNextLine) {
          val line = scanner.nextLine
          album = album:+line
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

  def loadAlbum(): Album = {
    val album = new Album(Nil)
    val list = readFile(path + "images/photoAlbum.txt")
    def aux(l: List[String], alb: Album): Album = {
      l match {
        case Nil => alb
        case x::xs => {
          val alb1: Album = alb.addImage(x)
          aux(xs, alb1)
        }
      }
    }
    aux(list.reverse, album)
  }
}