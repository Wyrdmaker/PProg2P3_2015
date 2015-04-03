import scala.swing._
import scala.swing.event._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat

import GUI._
import Games.AdvanceWars._

package Games{
package AdvanceWars{

abstract class AdvanceWars_Label_State extends Label_State[AdvanceWars_Label] {
	//val size_x = AdvanceWars.square_size_x
	//val size_y = AdvanceWars.square_size_y
	val opaque = true
	val foreground = AdWGE.black
}

class Label_State_Black extends AdvanceWars_Label_State{
	val state_name = "black"

	val label_border = AdWGE.border(AdWGE.black_dim)
	val background = AdWGE.label_color_black
	val text = ""
}

class Label_State_White extends AdvanceWars_Label_State {
	val state_name = "white"

	val label_border = AdWGE.border(AdWGE.black_dim)
	val background = AdWGE.label_color_white
	val text = ""
}

trait AdvanceWars_Label_States_Manager {
	val Label_State_Black = new Label_State_Black
	val Label_State_White = new Label_State_White

	def change_to_state(f_label: AdvanceWars_Label, state_name: String) = {
		f_label.state = state_name
		state_name match {
			case Label_State_Black.state_name => Label_State_Black.change_to_state(f_label)
			case Label_State_White.state_name => Label_State_White.change_to_state(f_label)
		}
	}
}

class AdvanceWars_Label extends Grid_Label with AdvanceWars_Label_States_Manager{
	state = "black"
	preferredSize = new Dimension(AdvanceWars.square_size_x, AdvanceWars.square_size_y)

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
		AdvanceWars.flip(x,y)
		AdvanceWars.launch_game_timer()
		//if (!AdvanceWars.playing) {println("label_start"); AdvanceWars.launch_game_timer()/*AdvanceWars.game_frame_content.timer_label.restart(new Date()); AdvanceWars.playing = true*/} //lance le timer au premier clic sur une case de l'utilisateur}
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

}	//accolade fermante du package AdvanceWars
}	//accolade fermante du package Games