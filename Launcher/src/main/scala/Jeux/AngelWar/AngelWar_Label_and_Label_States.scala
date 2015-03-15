import scala.swing._
import scala.swing.event._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat
import javax.swing.{ImageIcon, Icon}
import java.awt.image.BufferedImage

abstract class AngelWar_Label_State extends Label_State[AngelWar_Label] {
	//val size_x = AngelWar.square_size_x
	//val size_y = AngelWar.square_size_y
	val opaque = true
	val foreground = AWGE.black
}

class Label_State_BlackAngel extends AngelWar_Label_State{
	val state_name = "black"
	//icon = new ImageIcon("src/main/ressources/AngelWar/black_angel_vector.png")

	var myicon = new ImageIcon("src/main/ressources/AngelWar/black_angel_by_sandara-d7oontj.jpg")
	val img = myicon.getImage()
	//val mybi = new java.awt.image.BufferedImage(img.getWidth(null), img.getHeight(null), BufferedImage.TYPE_INT_ARGB)
	//val g = mybi.createGraphics()
	//g.drawImage(img, 0, 0, this.size.width, this.size.height, null)
	//val newicon = new ImageIcon(mybi)
	//icon = newicon
	def f_custom_painting (g: Graphics2D, l:Label) ={
		g.drawImage(img, 0, 0, l.size.width, l.size.height, null)		
	}
	custom_painting = f_custom_painting


	val label_border = AWGE.border(AWGE.black_dim)
	val background = AWGE.label_color_black
	val text = ""
}

class Label_State_WhiteAngel extends AngelWar_Label_State {
	val state_name = "white"

	//icon = new ImageIcon("src/main/ressources/AngelWar/white_angel_by_sandara-d7v34ye.jpg")

	var myicon = new ImageIcon("src/main/ressources/AngelWar/white_angel_by_sandara-d7v34ye.jpg")
	val img = myicon.getImage()
	def f_custom_painting (g: Graphics2D, l:Label) ={
		g.drawImage(img, 0, 0, l.size.width, l.size.height, null)		
	}
	custom_painting = f_custom_painting

	val label_border = AWGE.border(AWGE.black_dim)
	val background = AWGE.label_color_white
	val text = ""
}

trait AngelWar_Label_States_Manager {
	val Label_State_BlackAngel = new Label_State_BlackAngel
	val Label_State_WhiteAngel = new Label_State_WhiteAngel

	def change_to_state(f_label: AngelWar_Label, state_name: String) = {
		f_label.state = state_name
		state_name match {
			case Label_State_BlackAngel.state_name => Label_State_BlackAngel.change_to_state(f_label)
			case Label_State_WhiteAngel.state_name => Label_State_WhiteAngel.change_to_state(f_label)
		}
	}
}

class AngelWar_Label extends Grid_Label with AngelWar_Label_States_Manager{
	var state = "black" //valeur nécessaire pour que les AngelWar_Label puissent etre instanciés par Grid main, inutile sinon
	preferredSize = new Dimension(AngelWar.square_size_x, AngelWar.square_size_y)

	def init(colour: Boolean, influence_list: List[Boolean]) : Unit = {
		if (colour) {change_to_state(this, "white")}
		else {change_to_state(this, "black")}
		listenTo(mouse.moves, mouse.clicks)
	}

	override def mouse_enter_reaction () ={

	}
	override def mouse_exit_reaction () ={

	}
	override def mouse_leftclic_reaction () ={
		AngelWar.flip(x,y)
		AngelWar.launch_game_timer()
		//if (!AngelWar.playing) {println("label_start"); AngelWar.launch_game_timer()/*AngelWar.game_frame_content.timer_label.restart(new Date()); AngelWar.playing = true*/} //lance le timer au premier clic sur une case de l'utilisateur}
	}
	override def mouse_rightclic_reaction () = {

	}
	
	def turn (): Unit ={
		state match {
			case "black" => change_to_state(this, "white")
			case "white" => change_to_state(this, "black")
		}
	}


	/*ImageIcon icon = new ImageIcon("whatever.jpg");
	Make sure the reference you create is an ImageIcon reference. Then use getImage() to grab the image from the ImageIcon:
	Image img = icon.getImage();
	Now create a buffered image the same size as the image:
	BufferedImage bi = new BufferedImage(img.getWidth(null), img.getHeight(null), BufferedImage.TYPE_INT_ARGB);
	Then blit the icon image to the buffered image, and resize it as you do so:
	Graphics g = bi.createGraphics();
	g.drawImage(img, 0, 0, WIDTH, HEIGHT, null);
	(The code above may be incorrect - check the docs)
	Now recreate the IconImage with the new buffered image:
	IconImage newIcon = new IconImage(bi);*/

}
