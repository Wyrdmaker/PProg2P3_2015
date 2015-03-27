import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat
import java.awt.event.{ActionEvent, ActionListener}
//import javax.swing.{ImageIcon, Icon}

package GUI{

//DÉFAUT: le thread utilisé par le timer ne s'arrète pas, m^eme après avoir fermé le jeu (écrire un println("tick") dans la fonction actionPerformed)

case class Minute_Tick(minutes: Int, timer: Timer_Label) extends Event
case class Timer_Stop(timer_stopped: Timer_Label) extends Event

//La classe Timer_Label fournit des labels chronomètres dotés de 3 fonctions: 
//	restart -> change l'origine temporelle du chronomètre
//	start -> lance le chronomètre
//	stop -> arrete le chronomètre
class Timer_Label (time_origin_arg : Date) extends Label with Publisher{
	val this_timer_label = this
	var time_origin = time_origin_arg
	var minutes = ((new Date).getTime() - time_origin.getTime()) / 60000 % 60
	var secondes = ((new Date).getTime() - time_origin.getTime()) / 1000 % 60
	text = "00:00"

	def reset_text() ={
		text = "00:00"
	}

	def restart (new_time_origin: Date) = {
		time_origin = new_time_origin
		timer.start()
	}

	def start () = {
		timer.start()
	}

	def stop () = {
		timer.stop()
		publish(Timer_Stop(this_timer_label))
	}
	
	val timer_listener = new ActionListener{
		def actionPerformed(e: ActionEvent) {
			val old_minutes = minutes
			minutes  = ((new Date).getTime() - time_origin.getTime()) / 60000 % 60
			if(minutes > old_minutes){publish(Minute_Tick(minutes.toInt, this_timer_label))}
			secondes = ((new Date).getTime() - time_origin.getTime()) / 1000 % 60
			var string = if (minutes < 10) "0" else ""
			string = string + minutes.toString + ":"
			string = if (secondes < 10) string + "0" else string
			string = string + secondes.toString
			this_timer_label.text = string
		}
	}

	val timer = new javax.swing.Timer(1000, timer_listener)
}

//Voici une classe pour exécuter une action une fois, lorsque le délai sera écoulé:
//Exemple d'utilisation: "new Delayed_Action(500, Unit=> println("coucou"))"
class Delayed_Action(delay: Int, action_to_perform: (Unit=> Unit)){
	val timer_listener = new ActionListener{
		def actionPerformed(e: ActionEvent) {
			action_to_perform()
		}
	}
	val timer : javax.swing.Timer = new javax.swing.Timer(delay, timer_listener){setRepeats(false)}
	timer.start()	
}



}	//Accolade fermante du package GUI