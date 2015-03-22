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
	val background = TGE.label_color_unexplored
	val text = ""
	changing_text_enabled = false
}

class Label_State_1 extends Towers_Label_State {
	val state_name = "1"
	val text = ""
	changing_text_enabled = false
	val label_border = TGE.border(TGE.black)
	val background = TGE.label_color_unexplored
	override val foreground = TGE.black
}

class Label_State_0 extends Towers_Label_State {
        val state_name = "0"
        val background = TGE.firebrick4
        val text = ""
        val label_border = Swing.EmptyBorder(0)
        override val foreground = TGE.dark_golden_rod1
        override def change_to_state(t_label: Towers_Label) = {
                super.change_to_state(t_label)
                if (t_label.condition >= 0){t_label.text = t_label.condition.toString}
        }
}

/*
class Label_State_ extends Towers_Label_State {
	val state_name = "0"

	val label_border = TGE.border(TGE.black)
	val background = TGE.label_color_flagged
	val text = ""
	def f_custom_painting (g:Graphics2D, label:Label) = {
		if (Towers.color_parameter == "Océan") {
			val center = ((label.size.width/2).toInt, ((label.size.height/2).toInt))
			g.setColor(TGE.green)
			val radius = 15
			g.fillOval(center._1-(radius/2),center._2-(radius/2),radius,radius)
		}		
	}
	custom_painting = f_custom_painting
}
*/
trait Towers_Label_States_Manager {
	val Label_State_2 = new Label_State_2
	val Label_State_1 = new Label_State_1
	val Label_State_0 = new Label_State_0

	def change_to_state(t_label: Towers_Label, state_name: String) = {
		t_label.state = state_name
			state_name match {
			case Label_State_2.state_name => Label_State_2.change_to_state(t_label)
			case Label_State_1.state_name => Label_State_1.change_to_state(t_label)
			case Label_State_0.state_name => Label_State_0.change_to_state(t_label)
		}
	}
}

class Towers_Label extends Grid_Label with Towers_Label_States_Manager /*with Towers_Graphical_Elements*/{
	var assigned = false
	var value = 0
	var value_t = fill[Boolean](Towers.nb_of_rows)(true)
	var nb_val = 0
	var condition = 0
	var num = 0
	font = new Font("Arial", 1, 32) // 0 pour normal, 1 pour gras, 2 pour italique ...
	preferredSize = new Dimension(Towers.square_size_x, Towers.square_size_y)

	def init(tipe: Int, condition_value: Int = -1) : Unit = {
                //si la condition_value est différent de -1, alors c'est un label de condition
                condition = condition_value
                tipe match{
                        case 0 => change_to_state(this, "0")
                        case 1 => change_to_state(this, "1")
                        case 2 => change_to_state(this, "2")
                }
                listenTo(mouse.moves, mouse.clicks)
        }

	override def mouse_enter_reaction () = {
		if (state != "0")
                	border = TGE.highlighted_border
        }
        override def mouse_exit_reaction () = {
                if (state != "0") {
			if (state == "1")
				border = TGE.border(TGE.black)
			else
				border = TGE.border(TGE.red)
        	}
	}
        override def mouse_leftclic_reaction () ={
		if (state != "0") {
			num = (num + 1) % (Towers.nb_of_rows + 1)
                        text = num.toString
			if (text == "0") text = ""
		}
		grid_check()
        }
        override def mouse_rightclic_reaction () = {
        }

	def next_val() : Int = {
		var i = 0
		while (i < Towers.nb_of_rows && !value_t(i))
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
	
	def grid_check() = {
		var g = Towers.game_frame_content.grid
		var n = Towers.nb_of_rows
		var v = true
                g.get_contents.foreach(label => change_to_state(label, "1"))
                for (x <- 0 until n) {
                        var t: Array[Array[Int]] = Array.fill(n + 1){Array()}
                        for (y <- 0 until n)
                                t(g.access_xy(x, y).num) = t(g.access_xy(x, y).num):+ y
                        for (y <- 1 to n) {
                                if (t(y).length > 1) {
					v = false
                                        for (i <- 0 until t(y).length)
                                                change_to_state(g.access_xy(x, t(y)(i)), "2")
				}
				if (t(y).length == 0)
					v = false
			}
                }
                for (y <- 0 until n) {
                        var t: Array[Array[Int]] = Array.fill(n + 1){Array()}
                        for (x <- 0 until n)
                                t(g.access_xy(x, y).num) = t(g.access_xy(x, y).num):+ x
                        for (x <- 1 to n) {
                                if (t(x).length > 1) {
					v = false
                                        for (i <- 0 until t(x).length)
                                                change_to_state(g.access_xy(t(x)(i), y), "2")
					}
                                if (t(x).length == 0)
                                        v = false
			}
                }
		if (v)
			Towers.win()
        }

}

}

}
