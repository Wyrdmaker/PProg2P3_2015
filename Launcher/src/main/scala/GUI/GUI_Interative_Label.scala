import scala.swing._
import scala.swing.event._

package GUI{

class Interactive_Label extends Label{
	//Des Labels auxquels ont peut spécifier une méthode de peinture (la variable custom_painting), et dont on peut spécifier les
	//réponses à des évènements d'interactions avec la souris (quand il est cliqué, quand la souris entre ou sort de sa zone ...)

	var custom_painting: ((Graphics2D,Label) => Unit) = ((g:Graphics2D, uiel:UIElement) => ())
	override def paint(g: Graphics2D)={
		super.paint(g)
		custom_painting(g,this)
	}


	listenTo(mouse.moves, mouse.clicks)
	//Ces fonctions sont à overrider pour définir les réactions du labels face à différents évènements de souris
	def mouse_enter_reaction () ={	//Lorsque la souris entre dans la zone du label
	}
	def mouse_exit_reaction () ={	//Lorsque la souris quitte la zone du label
	}
	def mouse_leftclic_reaction () ={	//Lorsque le label est cliqué avec le clic gauche
	}
	def mouse_middleclic_reaction () ={	//Lorsque le label est cliqué avec le clic central
	}
	def mouse_rightclic_reaction () ={	//Lorsque le label est cliqué avec le clic droit
	}
	reactions += {
		case e: MouseEntered =>
			mouse_enter_reaction()
		case e: MouseExited =>
			mouse_exit_reaction()
		case e: MouseClicked =>
			e.peer.getButton match {
				case java.awt.event.MouseEvent.BUTTON1 =>
					mouse_leftclic_reaction()
				case java.awt.event.MouseEvent.BUTTON2 =>
					mouse_middleclic_reaction()
				case java.awt.event.MouseEvent.BUTTON3 =>
					mouse_rightclic_reaction()
			}					
	}	
}


}