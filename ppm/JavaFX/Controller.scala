package JavaFX

import javafx.fxml.FXML
import javafx.scene.SubScene
import javafx.scene.control.{Button, Label, MenuItem, TextField}
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout.GridPane
import qtree.Coords.Coords
import qtree.{MyRandom, QTree}
import qtree.QuadTree._

import java.io.FileInputStream

class Controller {

  @FXML
  private var imageNameLabel: Label = _

  @FXML
  private var previousButton: Button = _

  @FXML
  private var nextButton: Button = _

  @FXML
  private var img1: ImageView = _

  @FXML
  private var editButton: Button = _

  @FXML
  private var menuItemDelete: MenuItem = _

  @FXML
  private var menuItemAdd: MenuItem = _

  @FXML
  private var menuItemRotateE: MenuItem = _

  @FXML
  private var menuItemRotateD: MenuItem = _

  @FXML
  private var menuItemMirrorH: MenuItem = _

  @FXML
  private var menuItemMirrorV: MenuItem = _

  @FXML
  private var menuItemSepia: MenuItem = _

  @FXML
  private var menuItemContrast: MenuItem = _

  @FXML
  private var menuItemNoise: MenuItem = _

  @FXML
  private var menuItemScale: MenuItem = _

  @FXML
  private var buttonFile: Button = _

  @FXML
  private var labelWindow: Label = _

  @FXML
  private var menuExit: MenuItem = _

  @FXML
  private var textField_caminho: TextField = _

  @FXML
  private var popupSubScene: SubScene = _

  @FXML
  private var cancelButton: Button = _

  @FXML
  private var okButton: Button = _

  @FXML
  private var opcaoTextField: TextField = _

  @FXML
  var opcaoLabel: Label = _

  @FXML
  var deleteButton: Button = _

  @FXML
  var slideMenuItem: MenuItem = _

  @FXML
  var gridMenuItem: MenuItem = _

  @FXML
  var gridPanel: GridPane = _

  @FXML
  var imageGrid00: ImageView = _

  @FXML
  var imageGrid01: ImageView = _

  @FXML
  var imageGrid02: ImageView = _

  @FXML
  var imageGrid10: ImageView = _

  @FXML
  var imageGrid11: ImageView = _

  @FXML
  var imageGrid12: ImageView = _

  @FXML
  var moveMenuItem: MenuItem = _

  @FXML
  var buttonMove: Button = _


  var albumC = FxApp.album
  var imgID: Int = albumC.imgs.head._1
  var imgName: String = albumC.imgs.head._2
  val path: String = "ppm/"
  var pathImage: String = path + imgName

  // ---------------------------------------------------------------------------
  // --------------------------- View SlideShow  -------------------------------
  // ---------------------------------------------------------------------------

  def onSlideMenuItemAction(): Unit = {
    img1.setVisible(true)
    imageNameLabel.setVisible(true)
    gridPanel.setVisible(false)
    imageGrid00.setVisible(false)
    imageGrid01.setVisible(false)
    imageGrid02.setVisible(false)
    imageGrid10.setVisible(false)
    imageGrid11.setVisible(false)
    imageGrid12.setVisible(false)
  }


  def onButtonNextClicked(): Unit = {
    imgName = albumC.nextImage(imgID).get._2
    imgID = albumC.nextImage(imgID).get._1
    updateAlbum()
    println(albumC.nextImage(imgID))
  }

  def onButtonPreviousClicked(): Unit = {
    imgName = albumC.previousImage(imgID).get._2
    imgID = albumC.previousImage(imgID).get._1
    updateAlbum()
    println(albumC.previousImage(imgID))
  }

  def updateAlbum(): Unit = {
    pathImage = path + imgName
    img1.setImage(new javafx.scene.image.Image(new FileInputStream(pathImage)))
    imageNameLabel.setText(imgName.drop(path.length + 3).dropRight(4))
  }

  // ---------------------------------------------------------------------------
  // ----------------------------- View da Grelha   ----------------------------
  // ---------------------------------------------------------------------------

  def onGridMenuItemAction(): Unit = {
    img1.setVisible(false)
    imageNameLabel.setVisible(false)
    gridPanel.setVisible(true)
    imageGrid00.setVisible(true)
    imageGrid01.setVisible(true)
    imageGrid02.setVisible(true)
    imageGrid10.setVisible(true)
    imageGrid11.setVisible(true)
    imageGrid12.setVisible(true)
    imageGrid00.setImage((new javafx.scene.image.Image(new FileInputStream(pathImage))))
    onButtonNextClicked()
    imageGrid01.setImage((new javafx.scene.image.Image(new FileInputStream(pathImage))))
    onButtonNextClicked()
    imageGrid02.setImage((new javafx.scene.image.Image(new FileInputStream(pathImage))))
    onButtonNextClicked()
    imageGrid10.setImage((new javafx.scene.image.Image(new FileInputStream(pathImage))))
    onButtonNextClicked()
    imageGrid11.setImage((new javafx.scene.image.Image(new FileInputStream(pathImage))))
    onButtonNextClicked()
    imageGrid12.setImage((new javafx.scene.image.Image(new FileInputStream(pathImage))))
  }



  // ---------------------------------------------------------------------------
  // ----------------------------- Aux do MenuBar   ----------------------------
  // ---------------------------------------------------------------------------


  def editImage(f: QTree[Coords] => QTree[Coords]): Unit = {
    var change = convertToImage(f(convertToQTree()))
  }

  def convertToQTree(): QTree[Coords] = {
    val a1 = convertImgToArray(pathImage)
    val a2 = convertBitMapToList(a1)
    val a3 = makeQTree(a2)
    a3
  }

  def convertToImage(qt: QTree[Coords]): Unit = {
    val b2 = makeListList(qt)
    val b1 = convertListToBitmap(b2)
    val b3 = convertArraytoImg(b1, pathImage)
    img1.setImage((new javafx.scene.image.Image(new FileInputStream(pathImage))))
  }

  // ---------------------------------------------------------------------------
  // --------------------------- Items do MenuBar   ----------------------------
  // ---------------------------------------------------------------------------

  def onMenuItemRotateDClicked(): Unit = {
    editImage(rotateR)
  }

  def onMenuItemRotateEClicked(): Unit = {
    editImage(rotateL)
  }

  def onMenuItemMirrorHClicked(): Unit = {
    editImage(mirrorH)
  }

  def onMenuItemMirrorVClicked(): Unit = {
    editImage(mirrorV)
  }

  def onMenuItemSepiaClicked(): Unit = {
    convertToQTree()
    val change = convertToImage(mapColourEffect(sepiaEffect, convertToQTree()))
  }

  def onMenuItemContrastClicked(): Unit = {
    convertToQTree()
    val change = convertToImage(mapColourEffect(contrastEffect, convertToQTree()))
  }

  def onMenuItemNoiseClicked(): Unit = {
    convertToQTree()
    val change = convertToImage(mapColourEffect(noiseEffect, convertToQTree()))
  }

  def onMenuItemScaleClicked(): Unit = {
    convertToQTree()
    val change = convertToImage(scale(2, convertToQTree()))
  }


  // ---------------------------------------------------------------------------
  // --------------------------- Aparecimento 2a Janela  -----------------------
  // ---------------------------------------------------------------------------

  def showPopupAction(): Unit = {
    popupSubScene.setVisible(true)
    okButton.setVisible(true)
    cancelButton.setVisible(true)
    opcaoTextField.setVisible(true)
    opcaoLabel.setVisible(true)
  }


  def hidePopupAction(): Unit = {
    popupSubScene.setVisible(false)
    okButton.setVisible(false)
    cancelButton.setVisible(false)
    opcaoTextField.setVisible(false)
    opcaoLabel.setVisible(false)
    deleteButton.setVisible(false)
    buttonMove.setVisible(false)
  }

  def onMenuAddAction(): Unit = {
    showPopupAction()
    okButton.setText("Adicionar")
    cancelButton.setText("Cancelar")
    opcaoTextField.setText("images/sobreiros algarve.png")
    opcaoLabel.setText("Escreva o Caminho da Imagem")
  }

  def onMenuRemoveAction(): Unit = {
    showPopupAction()
    okButton.setVisible(false)
    cancelButton.setText("Cancelar")
    opcaoTextField.setVisible(false)
    opcaoLabel.setText("Tem a certeza que pretende Eliminar?")
    deleteButton.setVisible(true)
  }

  def onMenuMoveAction(): Unit = {
    showPopupAction()
    buttonMove.setVisible(true)
    cancelButton.setText("Cancelar")
    opcaoLabel.setText("Introduza a posicção a mover")
    opcaoTextField.setText("1")
  }

  def onButtonMoveClicked(): Unit = {
    albumC.swapImages(imgID, opcaoTextField.getText.toInt)
    hidePopupAction()
  }

  def onButtonCancelClicked(): Unit = {
    hidePopupAction()
  }

  def onButtonAdd(): Unit = {
    albumC = albumC.addImage(opcaoTextField.getText)
    println(albumC)
    imgID = albumC.findImage(1).get._1
    imgName = albumC.findImage(1).get._2
    println(imgName)
    img1.setImage(new javafx.scene.image.Image(new FileInputStream(path + imgName)))
    imageNameLabel.setText(imgName.drop(7).dropRight(4))
    println(path)
    FxApp.writeFile(albumC)
    hidePopupAction()
  }

  def onButtonDelete(): Unit = {
    albumC = albumC.removeImage(imgID)
    imgName = albumC.nextImage(imgID - 1).get._2
    imgID = albumC.nextImage(imgID - 1).get._1
    updateAlbum()
    FxApp.writeFile(albumC)
    println(albumC)
    hidePopupAction()
  }

  def setImage(str: String): Unit = {
    img1.setImage(new Image(str))
  }

  def setLabelName(str: String): Unit = {
    imageNameLabel.setText(str)
  }

}