import scala.swing._
import scala.swing.event._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat

import GUI._
import Games.Life._

package Games{
package Life{

abstract class Life_Label_State extends Label_State[Life_Label] {
	//val size_x = Life.square_size_x
	//val size_y = Life.square_size_y
	val opaque = true
	val foreground = LGE.black
	val label_border = Swing.EmptyBorder(1)
}

class Label_State_Alive extends Life_Label_State{
	val state_name = "alive"

	val background = LGE.label_color_black
	val text = ""
}

class Label_State_Dead extends Life_Label_State {
	val state_name = "dead"

	val background = LGE.label_color_white
	val text = ""
}

trait Life_Label_States_Manager {
	val Label_State_Alive = new Label_State_Alive
	val Label_State_Dead = new Label_State_Dead

	def change_to_state(f_label: Life_Label, state_name: String) = {
		f_label.state = state_name
		state_name match {
			case Label_State_Alive.state_name => Label_State_Alive.change_to_state(f_label)
			case Label_State_Dead.state_name => Label_State_Dead.change_to_state(f_label)
		}
	}
}

class Life_Label extends Grid_Label with Life_Label_States_Manager{
	state = "dead"
	preferredSize = new Dimension(Life.square_size_x, Life.square_size_y)

	def init(life: Boolean) : Unit = {
		if (life) {change_to_state(this, "alive")}
		else {change_to_state(this, "dead")}
		listenTo(mouse.moves, mouse.clicks)
	}
	override def mouse_leftclic_reaction () ={
		if(!Life.running){
			if(state=="dead"){change_to_state(this,"alive");/* println(Life.saved_board(x)(y)(0));*/ Life.board(x)(y)(0) = true;Life.board(x)(y)(1)=true/*; println(Life.saved_board(x)(y)(0))*/}
			else{change_to_state(this,"dead"); Life.board(x)(y)(0) = false; Life.board(x)(y)(1)=false}
		}
		//Life.launch_game_timer()
		//if (!Life.playing) {println("label_start"); Life.launch_game_timer()/*Life.game_frame_content.timer_label.restart(new Date()); Life.playing = true*/} //lance le timer au premier clic sur une case de l'utilisateur}
	}

	def next_state : String={
		if(Life.board(x)(y)(1)){return("alive")}
		else{return("dead")}
	}

	def evolve () ={
		change_to_state(this,next_state)
		Life.board(x)(y)(0)= (state=="alive")
	}
}

class Life_Border_Label extends Interactive_Label{
	background = LGE.firebrick4
	opaque = true

	var left_click_action: Unit => Unit = (Unit => Unit)
	override def mouse_leftclic_reaction () ={	
		if(Life.running){left_click_action()}
		else{left_click_action()}
	}
	def init(label_type: String)={
		label_type match{
			case "go/stop_label" =>{
				preferredSize = new Dimension(100,Life.square_size_y)
				border = Swing.LineBorder(LGE.dark_golden_rod1,1)
				foreground = LGE.dark_golden_rod1
				text = "Évolution"
				left_click_action = (Unit => {
					if(Life.running){text = "Évolution"; Life.running = false}
					else{text = "Pause"; Life.running = true}
				})
			}
			case "save_label" =>{
				preferredSize = new Dimension(100,Life.square_size_y)
				border = Swing.LineBorder(LGE.dark_golden_rod1,1)
				foreground = LGE.dark_golden_rod1
				text = "Sauvegarder"
				left_click_action = (Unit =>{
					Life.stop_evolution()
					Life.make_saved_board_as_board()
				})
			}
			case "load_label" =>{
				preferredSize = new Dimension(100,Life.square_size_y)
				border = Swing.LineBorder(LGE.dark_golden_rod1,1)
				foreground = LGE.dark_golden_rod1
				text = "Charger"
				left_click_action = (Unit =>{
						Life.stop_evolution()
						Life.make_board_as_saved_board()	
						Life.apply_board_to_the_grid()
				})
			}
			case "speed_label" =>{
				preferredSize = new Dimension(100,Life.square_size_y)
				border = Swing.LineBorder(LGE.dark_golden_rod1,1)
				foreground = LGE.dark_golden_rod1
				text = "Lent"
				val lent_tick_delay = 1000
				val normal_tick_delay = 350
				val rapide_tick_delay = 100
				left_click_action = (Unit =>{
						text match{
							case "Lent" =>{text = "Normal"; Life.change_tick_delay(normal_tick_delay)}
							case "Normal" =>{text = "Rapide"; Life.change_tick_delay(rapide_tick_delay)}
							case "Rapide" =>{text = "Lent"; Life.change_tick_delay(lent_tick_delay)}
						}			
				})	
			}
			case _ =>{}
		}
	}

}

}	//accolade fermante du package Life
}	//accolade fermante du package Games