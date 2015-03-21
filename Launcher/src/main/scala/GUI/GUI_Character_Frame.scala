import scala.swing._
import scala.swing.event._
import javax.swing.{ImageIcon, Icon}
import java.awt.image.BufferedImage
import scala.math._
import java.awt.event.{ActionEvent, ActionListener}

package GUI{

//caractères à copier-coller: espace insécable " ", "â", "ê"
object Larissa extends Frame{
	def say(text_to_say: String){	//Fait dire au personnage la chaine donnée en argument. Celle-ci peut contenir de l'html, notamment <br>
		if(random_gen.nextInt(100) == 0){/*Easter_Egg*/ balloon_label.text = html_prefix + "Antonin Penon n'est pas un palyndrome" + html_suffix}
		else{/*Normal*/balloon_label.text = html_prefix + text_to_say + html_suffix}
		balloon_flowpanel.border = Swing.LineBorder(GUI_GE.medium_violet_red, 10)
		new Delayed_Action(1000, Unit=> {balloon_flowpanel.border = Swing.LineBorder(GUI_GE.transparent, 10)})
	}

	def say_smth(word_list: Array[String]) = {	//Le personnage va choisir une des chaines de caractères du tableau et la dire
		if (word_list.length != 0) {
			say(word_list(random_gen.nextInt(word_list.length)))
		}
		else{println("Anormal: On a appelé la méthode say_smth_from du personnage avec une liste de paroles vide")}
	}

	title = "Larissa"
	iconImage = toolkit.getImage(getClass.getResource("/Larissa(cut).png"))
	peer.setDefaultCloseOperation(0);

	listenTo(this)
	reactions +={
		case e: WindowClosing => {
			say("Ce n'est pas très gentil.<br>Si vous voulez vraiment arrêter de jouer, fermez le jeu, puis le lanceur.")
		}
	}

	private val balloon_font = new Font("Utopia", 1, 22)
	private val balloon_foreground = GUI_GE.medium_violet_red

	private val random_gen = new scala.util.Random()
	private val html_prefix = "<html> <p> "
	private val html_suffix = "</p> </html>"

	private val chara_height = 344
	private val chara_width = 277
	private val chara_image = (new ImageIcon(getClass.getResource("/Larissa(cut).png"))).getImage()
	private val chara_x_offset = 25	//Définit l'abscisse à laquelle le personnage sera dessiné

	private val balloon_height = 267
	private val balloon_width = 400//384
	private val balloon_text_bottom_margin = 50//40
	private val balloon_text_left_margin = 50//30
	private val balloon_image = (new ImageIcon(getClass.getResource("/balloon2(mod).png"))).getImage()

	private var chara_panel: Panel = null
	private var left_balloon_label: Label = null
	private var balloon_label: Label = null
	private var balloon_flowpanel: FlowPanel = null
	private var boxpanel : BoxPanel = null

	def init() ={
		/*private val*/ chara_panel = new Panel{
			preferredSize = new Dimension(chara_width, chara_height)
			override def paint(g: Graphics2D)={
				super.paint(g)
				g.drawImage(chara_image, chara_x_offset,0,chara_width + chara_x_offset,chara_height,null)
			}		
		}

		/*private val*/ left_balloon_label = new Label(){	//Label tampon dans le balloon_flow_panel pour instaurer une marge à gauche du label contenant le texte du balloon
			minimumSize = new Dimension(balloon_text_left_margin, balloon_height)
		}

		/*private val*/ balloon_label = new Label(){
			font = balloon_font
			foreground = balloon_foreground
			text = ""
			preferredSize = new Dimension(balloon_width - balloon_text_left_margin, balloon_height - balloon_text_bottom_margin)
		}

		/*private val*/ balloon_flowpanel = new FlowPanel{
			contents += left_balloon_label
			contents += balloon_label
			border = Swing.LineBorder(GUI_GE.transparent,10)
			preferredSize = new Dimension(balloon_width, balloon_height)
			override def paint(g: Graphics2D)={
				super.paint(g)
				g.drawImage(balloon_image, 0,0,balloon_width,balloon_height,null)
			}
		}

		/*private val*/ boxpanel = new BoxPanel(Orientation.Vertical){
			contents += balloon_flowpanel
			contents += chara_panel
		}

		contents = boxpanel

		preferredSize = new Dimension(max(chara_width, balloon_width), chara_height + balloon_height)
		resizable = false
	}

	init()
}


}
