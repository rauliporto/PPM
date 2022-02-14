package qtree

import qtree.BitMap.BitMap
import qtree.Coords.Coords
import qtree.ImageUtil._
import qtree.Section.Section
import scala.annotation.tailrec

import java.awt.Color
import scala.util.Random

object QuadTree {


  //T1
  // 1 - Convert img to Array
  def convertImgToArray(imgSrc: String): BitMap = {
    val arrayImg: Array[Array[Int]] = readColorImage(imgSrc)
    val bitmapImg: BitMap = arrayImg
    bitmapImg
  }

  def convertArraytoImg(btm: BitMap, s: String) = {
    writeImage(btm, s, "png")
  }

  // 2 - Convert Array(Bitmap) to List
  def convertBitMapToList(btm: BitMap): List[List[Int]] = {
    verifyListList(btm.toList map (x => x.toList))
  }

  def verifyListList(lst: List[List[Int]]): List[List[Int]] = {
    lazy val maxSize: Int = lst(0).length.max(lst.length)
    lazy val maxSizeV = if (isPowerof2(maxSize)) maxSize else nextPowerof2(maxSize)
    aux1(aux2(lst, maxSizeV - lst(0).length), maxSizeV - lst.length)
  }

  @tailrec
  def aux1(l: List[List[Int]], i: Int): List[List[Int]] = {
    i match {
      case 0 => l
      case x => aux1(l ::: List(List.fill(l(0).length)(0)), x - 1)
    }
  }

  @tailrec
  def aux2(l: List[List[Int]], i: Int): List[List[Int]] = {
    i match {
      case 0 => l
      case y => aux2(l map (k => k ::: List(0)), y - 1)
    }
  }

  @tailrec
  def isPowerof2(value: Double): Boolean = {
    value match {
      case 1 => true
      case x => if (x % 2 != 0) false else isPowerof2(x / 2)
    }
  }

  def nextPowerof2(value: Double): Int = {
    if (isPowerof2(value)) (value * 2).toInt else nextPowerof2(value - 1)
  }

  def convertListToBitmap(lst: List[List[Int]]): BitMap = {
    lst.toArray map (x => x.toArray)
  }

  def makeQTree(lst: List[List[Int]]): QTree[Coords] = {
    def auxMakeQTree(l: List[List[Int]], crd: Coords): QTree[Coords] = {
      l match {
        case List() => QEmpty
        case List(List()) => QEmpty
        case List(List(0)) => QEmpty
        case x => if (uniqueColor(x)) QLeaf(crd, decodeColor(x)) else QNode(crd, auxMakeQTree(splitList(x, 1), getQuadCoords(crd, 1)), auxMakeQTree(splitList(x, 2), getQuadCoords(crd, 2)), auxMakeQTree(splitList(x, 3), getQuadCoords(crd, 3)), auxMakeQTree(splitList(x, 4), getQuadCoords(crd, 4)))
      }
    }
    auxMakeQTree(lst, getLocalCoords(lst))
  }

  def makeListList(qt: QTree[Coords]): List[List[Int]] = {
    def auxMakeListList(q: QTree[Coords]): List[List[Int]] = {
      q match {
        case QEmpty => Nil
        case QLeaf(section: Section) => leafToList(section)
        case QNode(coords, one, two, three, four) => merge(auxMakeListList(one), auxMakeListList(two), auxMakeListList(three), auxMakeListList(four))
      }
    }
    auxMakeListList(qt)
  }

  // Converte a folha em list
  def leafToList(section: Section): List[List[Int]] = {
    val x: Int = section._1._2._1 - section._1._1._1
    val y: Int = section._1._2._2 - section._1._1._2
    val cor = ImageUtil.encodeRgb(section._2.getRed, section._2.getGreen, section._2.getBlue)
    List.fill(x * y)(cor).grouped(x).toList
  }

  //Verifica se a matriz(List[List[Int]]) tem toda a mesma cor ou não
  def uniqueColor(lst: List[List[Int]]): Boolean = {
    def auxUniqueColor(l: List[Int]): Boolean = {
      l match {
        case List(0) => false
        case Nil => false
        case h :: Nil => true
        case h :: x :: t => if (h == x) auxUniqueColor(x :: t) else false
      }
    }
    auxUniqueColor(lst.flatten)
  }

  def decodeColor(lst: List[List[Int]]): Color = {
    new Color(ImageUtil.decodeRgb(lst(0)(0))(0), ImageUtil.decodeRgb(lst(0)(0))(1), ImageUtil.decodeRgb(lst(0)(0))(2))
  }

  def encodeColor(c: Color): List[List[Int]] = {
    List(List(ImageUtil.encodeRgb(c.getRed, c.getGreen, c.getBlue)))
  }

  def getLocalCoords(lst: List[List[Int]]): Coords = {
    ((0, 0), (lst(0).length, lst.length))
  }

  def getQuadCoords(crd: Coords, s: Int): Coords = {
    s match {
      case 1 => (crd._1, (crd._1._1 + ((crd._2._1 - crd._1._1) / 2), crd._1._2 + ((crd._2._1 - crd._1._1) / 2)))
      case 2 => ((crd._1._1 + ((crd._2._1 - crd._1._1) / 2), crd._1._2), (crd._2._1, (crd._1._2 + (crd._2._2 - crd._1._2) / 2)))
      case 3 => ((crd._1._1, (crd._1._2 + (crd._2._1 - crd._1._1) / 2)), (crd._1._1 + ((crd._2._2 - crd._1._2) / 2), crd._2._2))
      case 4 => ((crd._1._1 + ((crd._2._1 - crd._1._1) / 2), (crd._1._2 + (crd._2._1 - crd._1._1) / 2)), crd._2)
    }
  }

  def splitListH(lst: List[List[Int]], s: Int): List[List[Int]] = {
    s match {
      case 1 => lst.splitAt(lst.length / 2)._1
      case 2 => lst.splitAt(lst.length / 2)._2
    }
  }

  def splitListV(lst: List[List[Int]], s: Int): List[List[Int]] = {
    lst match {
      case Nil => Nil
      case h :: t => s match {
        case 1 => h.splitAt(h.length / 2)._1 :: splitListV(t, 1)
        case 2 => h.splitAt(h.length / 2)._2 :: splitListV(t, 2)
      }
    }
  }

  def splitList(lst: List[List[Int]], s: Int): List[List[Int]] = {
    s match {
      case 1 => splitListV(splitListH(lst, 1), 1)
      case 2 => splitListV(splitListH(lst, 1), 2)
      case 3 => splitListV(splitListH(lst, 2), 1)
      case 4 => splitListV(splitListH(lst, 2), 2)
    }
  }

  def mergeH(l1: List[List[Int]], l2: List[List[Int]]): List[List[Int]] = {
    l1 ::: l2
  }

  def mergeV(l1: List[List[Int]], l2: List[List[Int]]): List[List[Int]] = {
    (l1, l2) match {
      case (Nil, x) => x
      case (x, Nil) => x
      case ((h1 :: t1), (h2 :: t2)) => ((h1 ::: h2) :: mergeV(t1, t2))
    }
  }

  def merge(l1: List[List[Int]], l2: List[List[Int]], l3: List[List[Int]], l4: List[List[Int]]): List[List[Int]] = {
    mergeH(mergeV(l1, l2), mergeV(l3, l4))
  }

  // T2 -  value(value:Double, qt:QTree):QTree operação de ampliação/redução de uma
  // imagem, segundo o fator fornecido (por exemplo 1.5 ampliará a imagem
  //aumentando ambos os seus lados em 50%);

  def scale(value: Double, qt: QTree[Coords]): QTree[Coords] = {
    qt match {
      case QEmpty => QEmpty
      case QLeaf((coords: Coords, color: Color)) => QLeaf(multiCoords(value, coords), color)
      case QNode(coords, one, two, three, four) => QNode(multiCoords(value, coords), scale(value, one), scale(value, two), scale(value, three), scale(value, four))
    }
  }

  def multiCoords(v: Double, c: Coords): Coords = {
    (((c._1._1 * v.toInt), (c._1._2 * v.toInt)), ((c._2._1 * v.toInt), (c._2._2 * v.toInt)))
  }

  // T3 mirrorV / mirrorH (qt:QTree):QTree operações de espelhamento vertical e
  //horizontal
  def mirrorV[A](qt: QTree[A]): QTree[A] = {
    qt match {
      case QEmpty => QEmpty
      case QLeaf(section) => QLeaf(section)
      case QNode(coords, one, two, three, four) => QNode(coords, mirrorV(two), mirrorV(one), mirrorV(four), mirrorV(three))
    }
  }

  def mirrorH[A](qt: QTree[A]): QTree[A] = {
    qt match {
      case QEmpty => QEmpty
      case QLeaf(section) => QLeaf(section)
      case QNode(coords, one, two, three, four) => QNode(coords, mirrorH(three), mirrorH(four), mirrorH(one), mirrorH(two))
    }
  }

  // T4 - rotateD / rotateR (qt:QTree):QTree operações de rotação de 90 graus nos dois sentidos;
  //rodar para a direita
  def rotateR[A](qt: QTree[A]): QTree[A] = {
    qt match {
      case QEmpty => QEmpty
      case QLeaf(section) => QLeaf(section)
      case QNode(coords, one, two, three, four) => QNode(coords, rotateR(three), rotateR(one), rotateR(four), rotateR(two))
    }
  }

  //rodar para a esquerda
  def rotateL[A](qt: QTree[A]): QTree[A] = {
    qt match {
      case QEmpty => QEmpty
      case QLeaf(section) => QLeaf(section)
      case QNode(coords, one, two, three, four) => QNode(coords, rotateL(two), rotateL(four), rotateL(one), rotateL(three))
    }
  }

  // T5 - mapColourEffect(f:Colour => Colour, qt:QTree):QTree mapeamento
  def mapColourEffect[A](f: Color => Color, qt: QTree[A]): QTree[A] = {
    qt match {
      case QEmpty => QEmpty
      case QLeaf(section: Section) => QLeaf(section._1, f(section._2))
      case QNode(coords, one, two, three, four) => QNode(coords, mapColourEffect(f, one), mapColourEffect(f, two), mapColourEffect(f, three), mapColourEffect(f, four))
    }
  }

  def sepiaEffect(color: Color): Color = {
    val newColor: Color = new Color(255.min((color.getRed * 0.393 + color.getGreen * 0.769 + color.getBlue * 0.189).toInt), 255.min((color.getRed * 0.349 + color.getGreen * 0.686 + color.getBlue * 0.168).toInt), 255.min((color.getRed * 0.272 + color.getGreen * 0.534 + color.getBlue * 0.131).toInt));
    newColor
  }

  def contrastEffect(color: Color): Color = {
    val factor: Int = (259 * (128 + 255)) / (255 * (259 - 128))
    val r: Int = 0.max(255.min((factor * (color.getRed - 128) + 128)))
    val g: Int = 0.max(255.min((factor * (color.getGreen - 128) + 128)))
    val b: Int = 0.max(255.min((factor * (color.getBlue - 128) + 128)))
    val newColor: Color = new Color(r, g, b)
    newColor
  }

  def noiseEffect(color: Color): Color = {
    val noise = Random.nextInt(2)
    noise match {
      case 0 => color
      case 1 => new Color(0, 0, 0)
      case 2 => new Color(255, 255, 255)
    }
  }


  // Pure Ranndom
  def mapColourEffect1[A](f: (Color,Int) => Color, qt: QTree[A], value: RandomWithState): QTree[A] = {
    val (newValue,newRandom) = value.nextInt(20)
    qt match {
      case QEmpty => QEmpty
      case QLeaf(section: Section) => QLeaf(section._1, f(section._2,newValue))
      case QNode(coords, one, two, three, four) => QNode(coords, mapColourEffect1(f, one,newRandom), mapColourEffect1(f, two,newRandom), mapColourEffect1(f, three,newRandom), mapColourEffect1(f, four,newRandom))
    }
  }

  def noiseEffect1(color: Color,value: Int): Color = {
    value match {
      case 0 => color
      case 1 => new Color(0, 0, 0)
      case 2 => new Color(255, 255, 255)
    }
  }
}
