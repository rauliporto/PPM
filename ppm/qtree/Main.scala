package qtree

import qtree.QuadTree._

object Main {
  def main(args: Array[String]): Unit = {


    // ---------------------------------------------------------------------------
    // -------------------   Text User Interface  -------------------------------
    // ---------------------------------------------------------------------------
    val testeUser = new TextUserInterface()
    testeUser.interface()


    // ---------------------------------------------------------------------------
    // ------------------ Convert Img to QuadTree  -------------------------------
    // ---------------------------------------------------------------------------
    val nome_imagem: String = "ppm/images/imagem_casa"
    val tipo_ficheiro: String = ".png"

    //Convert img to BitMap
    val imagemArray = convertImgToArray(nome_imagem + tipo_ficheiro)

    //Convert BitMap to List
    val imgList = convertBitMapToList(imagemArray)

    //Convert List to Quadtree
    val imgQuadTree = makeQTree(imgList)

    // ---------------------------------------------------------------------------
    // ------------------ Convert QuadTree to Img  -------------------------------
    // ---------------------------------------------------------------------------

    // Convert QuadTree to List
    val quadToList = makeListList(imgQuadTree)

    // Convert List to BitMap
    val listToArray = convertListToBitmap(quadToList)

    // Convert BitMap to Img
    val arrayToImg = convertArraytoImg(listToArray, nome_imagem + "_QTree" + tipo_ficheiro)


    // ---------------------------------------------------------------------------
    // --------------------------- Scale QuadTree  -------------------------------
    // ---------------------------------------------------------------------------

    println("Scale")
    // Scale QuadTree
    val quadScale = scale(2.5, imgQuadTree)

    // Convert QuadTree to List
    val quadToListScale = makeListList(quadScale)

    // Convert List to BitMap
    val listToArrayScale = convertListToBitmap(quadToListScale)

    // Save Img Scaled
    val imgScale = convertArraytoImg(listToArrayScale, nome_imagem + "_Scaled" + tipo_ficheiro)
    println("End Scale")

    // ---------------------------------------------------------------------------
    // -------------------------- Rotate QuadTree  -------------------------------
    // ---------------------------------------------------------------------------
    println("Rotate")
    // Rotate to Left QuadTree
    val quadRotateL = rotateL(imgQuadTree)

    // Convert QuadTree to List
    val quadToListRotateL = makeListList(quadRotateL)

    // Convert List to BitMap
    val listToArrayRotateL = convertListToBitmap(quadToListRotateL)

    // Save Img Rotated Left
    val imgRotateL = convertArraytoImg(listToArrayRotateL, nome_imagem + "_RotatedL" + tipo_ficheiro)


    // Rotate to Right QuadTree
    val quadRotateR = rotateR(imgQuadTree)

    // Convert QuadTree to List
    val quadToListRotateR = makeListList(quadRotateR)

    // Convert List to BitMap
    val listToArrayRotateR = convertListToBitmap(quadToListRotateR)

    // Save Img Rotated Right
    val imgRotateR = convertArraytoImg(listToArrayRotateR, nome_imagem + "_RotatedR" + tipo_ficheiro)


    println("End Rotate")

    // ---------------------------------------------------------------------------
    // -------------------------- Mirror QuadTree  -------------------------------
    // ---------------------------------------------------------------------------
    println("Mirror")
    // Mirror Vertical QuadTree
    val quadMirroV = mirrorV(imgQuadTree)

    // Convert QuadTree to List
    val quadToListMirrorV = makeListList(quadMirroV)

    // Convert List to BitMap
    val listToArrayMirrorV = convertListToBitmap(quadToListMirrorV)

    // Save Img Mirror Vertical
    val imgMirrorV = convertArraytoImg(listToArrayMirrorV, nome_imagem + "_MirrorV" + tipo_ficheiro)

    // ---------------------------------------------------------------------------

    // Mirror Horizontal QuadTree
    val quadMirroH = mirrorH(imgQuadTree)

    // Convert QuadTree to List
    val quadToListMirrorH = makeListList(quadMirroH)

    // Convert List to BitMap
    val listToArrayMirrorH = convertListToBitmap(quadToListMirrorH)

    // Save Img Mirror Horizontal
    val imgMirrorH = convertArraytoImg(listToArrayMirrorH, nome_imagem + "_MirrorH" + tipo_ficheiro)

    println("End Mirror")
    // ---------------------------------------------------------------------------
    // --------------------------- Effects Qtree  --------------------------------
    // ---------------------------------------------------------------------------
    println("Effect")

    // Sepia Effect
    val sepiaQtree = mapColourEffect(sepiaEffect, imgQuadTree)

    // Convert QuadTree to List
    val quadToListSepia = makeListList(sepiaQtree)

    // Convert List to BitMap
    val listToArraySepia = convertListToBitmap(quadToListSepia)

    // Save Img Sepia
    val imgSepia = convertArraytoImg(listToArraySepia, nome_imagem + "_Effect_Sepia" + tipo_ficheiro)


    // Contrast Effect
    val contrastQtree = mapColourEffect(contrastEffect, imgQuadTree)

    // Convert QuadTree to List
    val quadToListContrast = makeListList(contrastQtree)

    // Convert List to BitMap
    val listToArrayContrast = convertListToBitmap(quadToListContrast)

    // Save Img Contrast
    val imgContrast = convertArraytoImg(listToArrayContrast, nome_imagem + "_Effect_Contrast" + tipo_ficheiro)

    // Noise Effect
    val noiseQtree = mapColourEffect(noiseEffect, imgQuadTree)

    // Convert QuadTree to List
    val quadToListNoise = makeListList(noiseQtree)

    // Convert List to BitMap
    val listToArrayNoise = convertListToBitmap(quadToListNoise)

    // Save Img Noise
    val imgNoise = convertArraytoImg(listToArrayNoise, nome_imagem + "_Effect_Noise" + tipo_ficheiro)


  }
}
