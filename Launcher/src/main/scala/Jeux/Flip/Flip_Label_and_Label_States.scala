import scala.swing._
import scala.swing.event._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat

import GUI._
import Games.Flip._

package Games{
package Flip{

abstract class Flip_Label_State extends Label_State[Flip_Label] {
	//val size_x = Flip.square_size_x
	//val size_y = Flip.square_size_y
	val opaque = true
	val foreground = FGE.black
}

class Label_State_Black extends Flip_Label_State{
	val state_name = "black"

	val label_border = FGE.border(FGE.black_dim)
	val background = FGE.label_color_black
	val text = ""
}

class Label_State_White extends Flip_Label_State {
	val state_name = "white"

	val label_border = FGE.border(FGE.black_dim)
	val background = FGE.label_color_white
	val text = ""
}

trait Flip_Label_States_Manager {
	val Label_State_Black = new Label_State_Black
	val Label_State_White = new Label_State_White

	def change_to_state(f_label: Flip_Label, state_name: String) = {
		f_label.state = state_name
		state_name match {
			case Label_State_Black.state_name => Label_State_Black.change_to_state(f_label)
			case Label_State_White.state_name => Label_State_White.change_to_state(f_label)
		}
	}
}

class Flip_Label extends Grid_Label with Flip_Label_States_Manager{
	//var state = "black" //valeur nécessaire pour que les Flip_Label puissent etre instanciés par Grid main, inutile sinon
	preferredSize = new Dimension(Flip.square_size_x, Flip.square_size_y)

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
		Flip.flip(x,y)
		Flip.launch_game_timer()
		//if (!Flip.playing) {println("label_start"); Flip.launch_game_timer()/*Flip.game_frame_content.timer_label.restart(new Date()); Flip.playing = true*/} //lance le timer au premier clic sur une case de l'utilisateur}
	}
	override def mouse_rightclic_reaction () = {

	}
	
	def turn (): Unit ={
		state match {
			case "black" => change_to_state(this, "white")
			case "white" => change_to_state(this, "black")
		}
	}
}

}	//accolade fermante du package Flip
}	//accolade fermante du package Games