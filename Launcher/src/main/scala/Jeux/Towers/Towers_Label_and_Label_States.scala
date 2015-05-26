import scala.swing._
import scala.swing.event._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import scala.Array._

import Games.Towers._
import GUI._

package Games{
package Towers{


abstract class Towers_Label_State extends Label_State[Towers_Label] {
	val foreground = TGE.black
	val opaque = true
}

class Label_State_2 extends Towers_Label_State{
	val state_name = "2"
	val label_border = TGE.border(TGE.red)
	override val foreground = TGE.red
	val background = TGE.background_colour
	val text = ""
	changing_text_enabled = false
}

class Label_State_1 extends Towers_Label_State {
	val state_name = "1"
	val text = ""
	changing_text_enabled = false
	val label_border = TGE.border(TGE.black)
	val background = TGE.background_colour
	override val foreground = TGE.normal_foreground_colour//TGE.black
}

trait Towers_Label_States_Manager {
	val Label_State_2 = new Label_State_2
	val Label_State_1 = new Label_State_1

	def change_to_state(t_label: Towers_Label, state_name: String) = {
		t_label.state = state_name
			state_name match {
			case Label_State_2.state_name => Label_State_2.change_to_state(t_label)
			case Label_State_1.state_name => Label_State_1.change_to_state(t_label)
		}
	}
}

class Towers_Label extends Grid_Label with Towers_Label_States_Manager /*with Towers_Graphical_Elements*/{
	var assigned = false
	var value = 0
	var value_t = fill[Boolean](Towers.size)(true)
	var nb_val = 0
	var condition = 0
	var num = 0
	font = new Font("Arial", 1, 32) // 0 pour normal, 1 pour gras, 2 pour italique ...
	preferredSize = new Dimension(Towers.square_size_x, Towers.square_size_y)

	// Valeur à la création
	def default() : Unit = {
		assigned = false
        	value = 0
        	value_t = fill[Boolean](Towers.size)(true)
        	nb_val = 0
        	condition = 0
        	num = 0
	}

	// Enlever les résultats du solver
	def clean() : Unit = {
		text = ""
		num = 0
		Towers.grid_check()
	}

	// Initialisation des cases
	def init() : Unit = {
                condition = -1
		num = 0
		text = value.toString
                change_to_state(this, "1")
                listenTo(mouse.moves, mouse.clicks)
        }
	
	// Initialisation des bordures
	def border_init(condition_value: Int) : Unit = {
                condition = condition_value
		opaque = true
        	background = TGE.firebrick4
		if (condition > 0)
			text = condition.toString
		border = Swing.EmptyBorder(0)
		foreground = TGE.dark_golden_rod1
        }
	
	// Réaction à la souris
	override def mouse_enter_reaction () = {
		if (condition < 0)
                	border = TGE.highlighted_border
        }
        override def mouse_exit_reaction () = {
                if (condition < 0) {
			if (state == "1")
				border = TGE.border(TGE.black)
			else
				border = TGE.border(TGE.red)
        	}
	}
        override def mouse_leftclic_reaction () ={
		if (condition < 0) {
			num = (num + 1) % (Towers.size + 1)
                        text = num.toString
			if (text == "0") text = ""
			Towers.grid_check()
		}
        }
        override def mouse_rightclic_reaction () = {
		if (condition < 0) {
                        num = (Towers.size + num) % (Towers.size + 1)
                        text = num.toString
                        if (text == "0") text = ""
                	Towers.grid_check()
		}
        }
	
	// Attribuer une valeur à chaque case bien comme il faut
	def next_val() : Int = {
		var i = 0
		while (i < Towers.size && !value_t(i))
			i += 1
		nb_val += 1
		value_t(i) = false
		i
	}

	def affect() : Unit = {
		var v = next_val()
                value = v + 1
		assigned = true
                Towers.suppress_value(v, x, y)
	}
}

}

}
