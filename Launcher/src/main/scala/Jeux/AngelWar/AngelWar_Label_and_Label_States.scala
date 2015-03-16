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
	val margin = 2
}

class Label_State_Empty extends AngelWar_Label_State{
	val state_name = "empty"
	val background = AWGE.sandy_brown
	val text = ""
	val label_border = AWGE.border(AWGE.black, 1)
}

class Label_State_Tent extends AngelWar_Label_State{
	val state_name = "tent"
	val background = AWGE.sandy_brown
	val text = ""
	val label_border = AWGE.border(AWGE.black, 1)
	var myicon = new ImageIcon("src/main/ressources/AngelWar/white_angel_by_sandara-d7v34ye.jpg")
	val img = myicon.getImage()
	def f_custom_painting (g: Graphics2D, l:Label) ={
		g.drawImage(img, margin, margin, l.size.width - 2*margin, l.size.height - 2*margin, null)		
	}
	custom_painting = f_custom_painting
}

class Label_State_Tree extends AngelWar_Label_State{
	val state_name = "tree"
	val background = AWGE.sandy_brown
	val text = ""
	val label_border = AWGE.border(AWGE.black, 1)
	var myicon = new ImageIcon("src/main/ressources/AngelWar/black_angel_by_sandara-d7oontj.jpg")
	val img = myicon.getImage()
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
			case 0 => {change_to_state(this, "empty")/*; println("init")*/}
			case 1 => {change_to_state(this, "tree")/*; println("init")*/}
			case 2 => {change_to_state(this, "tent")/*; println("init")*/}
			case 3 => {change_to_state(this, "condition")/*; println("init")*/}
		}
		listenTo(mouse.moves, mouse.clicks)
	}

	def set_adj_tent_error()={
		if(!adj_tent_error){
			border = AWGE.border(AWGE.red, 3)
			adj_tent_error = true
			AngelWar.error_nb = AngelWar.error_nb + 1
		}
	}
	def unset_adj_tent_error() ={
		if(adj_tent_error){
		change_to_state(this, state)
		adj_tent_error = false
		AngelWar.error_nb = AngelWar.error_nb - 1			
		}
	}

	def set_no_adj_tree_error()={
		if(!no_adj_tree_error){
			border = AWGE.border(AWGE.red, 3)
			no_adj_tree_error = true
			AngelWar.error_nb = AngelWar.error_nb + 1
		}		
	}
	def unset_no_adj_tree_error() ={
		if(no_adj_tree_error){
		change_to_state(this, state)
		no_adj_tree_error = false
		AngelWar.error_nb = AngelWar.error_nb - 1			
		}
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

	override def mouse_enter_reaction () ={
	}
	override def mouse_exit_reaction () ={
	}
	override def mouse_leftclic_reaction () ={
		state match{
			case "empty" => if(!locked){change_to_state(this,"tent"); AngelWar.add_tent(x,y)}
			case "tent" => {change_to_state(this, "empty"); AngelWar.remove_tent(x,y)}
			case _ => ()
		}
	}
	override def mouse_rightclic_reaction () = {
		state match{
			case "empty" => if(locked){locked=false; background = AWGE.sandy_brown}else{locked=true; background = locked_background}
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
