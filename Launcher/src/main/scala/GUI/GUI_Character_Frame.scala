import scala.swing._
import javax.swing.{ImageIcon}
import java.awt.image.BufferedImage

package GUI{

class Character_Frame (character: Character) extends Frame{
	var text : String = null
	println("height: " + character.height)
	println("width: " + character.width)
}

abstract class Character {
	val path_to_img: String
	val chara_imgicon = new ImageIcon(path_to_img)
	val chara_image = chara_imgicon.getImage()
	val height = chara_imgicon.getIconHeight()
	val width = chara_imgicon.getIconWidth()
}

object Larissa extends Character{
	val path_to_img = "src/main/ressources/Larissa.png"
}

}