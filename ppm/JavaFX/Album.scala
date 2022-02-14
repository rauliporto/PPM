package JavaFX

import JavaFX.Album.{Image, Images}

case class Album(imgs: Images) {
  def addImage(str: String): Album = Album.addImage(this, str)

  def findImage(id: Int): Option[Image] = Album.findImage(id, this.imgs)

  def nextImage(id: Int): Option[Image] = Album.nextImage(id, this)

  def previousImage(id: Int): Option[Image] = Album.previousImage(id, this)

  def removeImage(id: Int): Album = Album.removeImage(id, this)

  def swapImages(id1: Int, id2: Int): Unit = Album.swapImages(id1, id2, this)
}

object Album {
  type ID = Int
  type Path = String
  type Image = (ID, Path)
  type Images = List[Image]

  def addImage(album: Album, path: Path): Album = {
    def aux1(lstimg: Images, p: Path) = {
      lstimg match {
        case Nil => List((1, path))
        case _ => (lstimg.length + 1, path) :: lstimg
      }
    }
    val alb1 = new Album(aux1(album.imgs, path))
    sortAlbum(alb1)
  }

  def findImage(id: Int, lstimg: Images): Option[Image] = {
    lstimg find (x => x._1 == id)
  }

  def removeImage(id: Int, al: Album): Album = {
    def aux(i: Int, lstimg: List[Image]): List[Image] = {
      lstimg match {
        case Nil => Nil
        case h :: t => if (h._1 == i) t else h :: aux(i, t)
      }
    }
    val alb: Album = new Album(aux(id, al.imgs))
    sortAlbum(alb)
  }

  def sortAlbum(al: Album): Album = {
    val len = al.imgs.length
    def aux(lstimg: Images): Images = {
      lstimg match {
        case Nil => Nil
        case h :: t => (len - lstimg.length + 1, h._2) :: aux(t)
      }
    }
    val alb = new Album(aux(al.imgs))
    alb
  }

  def nextImage(id: Int, al: Album): Option[Image] = {
    if (id == al.imgs.length) findImage(1, al.imgs) else findImage(id + 1, al.imgs)
  }

  def previousImage(id: Int, al: Album): Option[Image] = {
    if (id == 1) findImage(al.imgs.length, al.imgs) else findImage(id - 1, al.imgs)
  }

  def swapImages(id1: Int, id2: Int, al: Album): Unit = {
    if (id1 <= al.imgs.length && id2 <= al.imgs.length && id1 > 0 && id2 > 0) {
      val img1 = al.imgs(id1-1)
      val img2 = al.imgs(id2-1)
     // if (false) {
        removeImage(id1, al)
        addImage(al, img2._2)
        removeImage(id2, al)
        addImage(al, img1._2)
   //   }
    }
    else
      println("Valor fora do intrevalo")
  }

}