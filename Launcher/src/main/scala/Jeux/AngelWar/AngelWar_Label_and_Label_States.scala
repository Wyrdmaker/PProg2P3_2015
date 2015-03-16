import scala.swing._
import scala.swing.event._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat
import javax.swing.{ImageIcon, Icon}
import java.awt.image.BufferedImage

import Games.AngelWar._
import GUI._

package Games{
package AngelWar{

abstract class AngelWar_Label_State extends Label_State[AngelWar_Label] {
	//val size_x = AngelWar.square_size_x
	//val size_y = AngelWar.square_size_y
	val opaque = true
	val foreground = AWGE.black
	val margin = 2
	val hell_background_icon = new ImageIcon("src/main/ressources/AngelWar/k2908634.jpg")
	val hell_background_image = hell_background_icon.getImage()
	val heaven_background_icon = new ImageIcon("src/main/ressources/AngelWar/heaven_abstract-blue.jpg")
	val heaven_background_image = heaven_background_icon.getImage()
	val white_angel_icon = new ImageIcon("src/main/ressources/AngelWar/white_angel_by_sandara-d7v34ye(reduite).jpg")
	val white_angel_image = white_angel_icon.getImage()
	val black_angel_icon = new ImageIcon("src/main/ressources/AngelWar/black_angel_by_sandara-d7oontj(reduite).jpg")
	val black_angel_image = black_angel_icon.getImage()
}

class Label_State_Empty extends AngelWar_Label_State{
	val state_name = "empty"
	val background = AWGE.sandy_brown
	val text = ""
	val label_border = AWGE.border(AWGE.black, 2)
	var img = hell_background_image
	AWGE.no_color_mode() match {
		case 1 => img = heaven_background_image
		case 0 => img = hell_background_image
		case _ => img = hell_background_image
	}
	def f_custom_painting (g: Graphics2D, l:Label) ={
		g.drawImage(img, margin, margin, l.size.width - 2*margin, l.size.height - 2*margin, null)		
	}
	custom_painting = f_custom_painting	
}

class Label_State_Tent extends AngelWar_Label_State{
	val state_name = "tent"
	val background = AWGE.sandy_brown
	val text = ""
	val label_border = AWGE.border(AWGE.black, 2)
	var img = white_angel_image
	AWGE.no_color_mode() match {
		case 1 => img = black_angel_image
		case 0 => img = white_angel_image
		case _ => img = white_angel_image
	}
	def f_custom_painting (g: Graphics2D, l:Label) ={
		g.drawImage(img, margin, margin, l.size.width - 2*margin, l.size.height - 2*margin, null)		
	}
	custom_painting = f_custom_painting
}

class Label_State_Tree extends AngelWar_Label_State{
	val state_name = "tree"
	val background = AWGE.sandy_brown
	val text = ""
	val label_border = AWGE.border(AWGE.black, 2)
	var img = black_angel_image
	AWGE.no_color_mode() match {
		case 1 => img = white_angel_image
		case 0 => img = black_angel_image
		case _ => img = black_angel_image
	}
	def f_custom_painting (g: Graphics2D, l:Label) ={
		g.drawImage(img, margin, margin, l.size.width - 2*margin, l.size.height - 2*margin, null)		
	}
	custom_painting = f_custom_painting
}

class Label_State_Condition extends AngelWar_Label_State{
	val state_name = "condition"
	val background = AWGE.firebrick4
	val text = ""
	val label_border = Swing.EmptyBorder(0)
	override val foreground = AWGE.dark_golden_rod1
	override def change_to_state(aw_label: AngelWar_Label) = {
		super.change_to_state(aw_label)
		if (aw_label.condition >= 0){aw_label.text = aw_label.condition.toString}
	}
}

trait AngelWar_Label_States_Manager {
	val Label_State_Empty = new Label_State_Empty
	val Label_State_Tent = new Label_State_Tent
	val Label_State_Tree = new Label_State_Tree
	val Label_State_Condition = new Label_State_Condition

	def change_to_state(f_label: AngelWar_Label, state_name: String) = {
		f_label.state = state_name
		state_name match {
			case Label_State_Empty.state_name => Label_State_Empty.change_to_state(f_label)
			case Label_State_Tent.state_name => Label_State_Tent.change_to_state(f_label)
			case Label_State_Tree.state_name => Label_State_Tree.change_to_state(f_label)
			case Label_State_Condition.state_name => Label_State_Condition.change_to_state(f_label)
			case _ => println("nom d'état incorrect pour changement d'état")
		}
	}
}

class AngelWar_Label extends Grid_Label with AngelWar_Label_States_Manager{
	var state = "empty" //valeur nécessaire pour que les AngelWar_Label puissent etre instanciés par Grid main, inutile sinon
	preferredSize = new Dimension(AngelWar.square_size_x, AngelWar.square_size_y)
	var condition = 0
	var adj_tent_error = false
	var no_adj_tree_error = false
	var condition_error = false
	var locked = false //vrai si le joueur a fait un clic droit sur ce label pour signaler que celui-ci ne devrait pas contenir de tente
	val locked_background = AWGE.chocolate4
	def init(tipe:Int, condition_value:Int = -1) : Unit = {
		//si la condition_value est différent de -1, alors c'est un label de condition
		condition = condition_value
		tipe match{
			case 0 => {change_to_state(this, "empty")/*; println("changing_to_empty: " + x + ", " + y)*/}
			case 1 => {change_to_state(this, "tree")/*; println("init")*/}
			case 2 => {change_to_state(this, "tent")/*; println("init")*/}
			case 3 => {change_to_state(this, "condition")/*; println("init")*/}
		}
		adj_tent_error = false
		no_adj_tree_error = false
		listenTo(mouse.moves, mouse.clicks)
		//println(state + ", " + x + ", " + y)
	}

	def set_adj_tent_error()={
		//print("set_adj_tent_error, " + x + ", " + y)
		if(!adj_tent_error /*&& state=="tent"*/){
			//print("real_change")
			//border = AWGE.border(AWGE.red, 3)
			adj_tent_error = true
			apply_errors()

			AngelWar.error_nb = AngelWar.error_nb + 1
		}
		//println()
	}
	def unset_adj_tent_error() ={
		//print("unset_adj_tent_error, " + x + ", " + y)
		if(adj_tent_error /*&& state=="tent"*/){
			//print("real_change")
			//change_to_state(this, state)
			adj_tent_error = false
			apply_errors()

			AngelWar.error_nb = AngelWar.error_nb - 1			
		}
		//println()
	}

	def set_no_adj_tree_error()={
		//print("set_no_adj_tree_error, " + x + ", " + y)
		if(!no_adj_tree_error /*&& state=="tent"*/){
			//print("real_change")
			//border = AWGE.border(AWGE.red, 3)
			no_adj_tree_error = true
			apply_errors()
			AngelWar.error_nb = AngelWar.error_nb + 1
		}
		//println()
	}
	def unset_no_adj_tree_error() ={
		//print("unset_no_adj_tree_error, " + x + ", " + y)
		if(no_adj_tree_error /*&& state=="tent"*/){
			//print("real_change")
			//change_to_state(this, state)
			no_adj_tree_error = false
			apply_errors()

			AngelWar.error_nb = AngelWar.error_nb - 1			
		}
		//println()
	}

	def set_condition_error()={
		if(state == "condition"){
			if(!condition_error){
				foreground = AWGE.red
				condition_error = true
				AngelWar.error_nb = AngelWar.error_nb + 1
			}

		}
	}
	def unset_condition_error()={
		if(state == "condition"){
			if(condition_error){
				change_to_state(this, "condition")
				condition_error = false
				AngelWar.error_nb = AngelWar.error_nb - 1			
			}

		}
	}
	def apply_errors() = {
		//A appeler par le label pour applique les conséquences graphiques des erreurs
		if(no_adj_tree_error || adj_tent_error){
			border = AWGE.border(AWGE.red, 3)
		}
		else {change_to_state(this, state)}
	}

	val hell_locked_icon = new ImageIcon("src/main/ressources/AngelWar/n_a7(transparent).png")
	val hell_locked_image = hell_locked_icon.getImage()
	val heaven_locked_icon = new ImageIcon("src/main/ressources/AngelWar/halo(transparent).png")
	val heaven_locked_image = heaven_locked_icon.getImage()

	def locked_custom_painting(g:Graphics2D, l:Label)={
		var img = AWGE.no_color_mode() match{
			case 1 => hell_locked_image
			case 0 => heaven_locked_image
			case _ => heaven_locked_image	
		}
		g.drawImage(img, 5, 5, l.size.width - 10, l.size.height - 10, null)		
	}	

	def add_to_custom_painting(f: (Graphics2D, Label) => Unit) = {
		val old_custom_painting = custom_painting
		def new_custom_painting(g:Graphics2D, l:Label) = {
			old_custom_painting(g, l)
			f(g,l)
		}
		custom_painting = new_custom_painting
		repaint()
	}

	override def mouse_enter_reaction () ={
	}
	override def mouse_exit_reaction () ={
	}
	override def mouse_leftclic_reaction () ={
		state match{
			case "empty" => if(!locked){change_to_state(this,"tent"); AngelWar.add_tent(x,y); apply_errors()}
			case "tent" => {change_to_state(this, "empty"); AngelWar.remove_tent(x,y); unset_no_adj_tree_error(); unset_adj_tent_error()}
			case _ => ()
		}
	}
	override def mouse_rightclic_reaction () = {
		state match{
			case "empty" => {
				if(locked){
					locked=false
					change_to_state(this, "empty")
					repaint()
				}
				else{
					locked=true
					//custom_painting = locked_custom_painting

					//add_to_custom_painting(create_locked_custom_painting())
					add_to_custom_painting(locked_custom_painting)
				}
			}
			case _ => ()
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

}	//accolade fermante du package AngelWar
}	//accolade fermante du package Games
