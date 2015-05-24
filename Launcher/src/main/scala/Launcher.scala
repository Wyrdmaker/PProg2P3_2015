import scala.swing._
import scala.math._
import scala.swing.event._
import javax.swing.{ImageIcon, Icon}
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.border
import java.awt.GradientPaint
import java.io._

import Games._
import GUI._

object Main {
	//var game_list = new File("src/main/scala/Jeux").listFiles.filter(_.isDirectory).map(_.getName)

	/* */var game_list = Array(AngelWar.AngelWar, Flip.Flip, Demineur.Demineur, Life.Life, Towers.Towers)/* */

	//Pour ajouter des jeux, ajouter dans le dossier Jeux un dossier contenant les fichiers <nom_du_jeu>.scala et
	// <nom_du_jeu>_Label_and_Label_States.scala. Le dossier ajouté doit porter le meme nom que le nom de la classe 
	//étendant la classe Game dans le jeu ajouté.
	var launcher_mainframe : MainFrame = null
	def main(args: Array[String]) {
		launcher_mainframe = new MainFrame{
			iconImage = toolkit.getImage(getClass.getResource("/my_purple_dice_20.png"))
			centerOnScreen()
			title = "Lanceur TDGH"
		}
		launcher_mainframe.visible = true

		if (game_list.length == 0) {
			println("Aucun jeu dans game_list de Launcher.scala")
			launcher_mainframe.contents = new Label("Désolé, il n'y a aucun jeu à lancer..."){
				preferredSize = new Dimension(300, 300)
			}
		}
		else {
			launcher_mainframe.contents = new Launcher_Content
			launcher_mainframe.centerOnScreen()
		}
		launcher_mainframe.dispose()	//dispose() permet de renvoyer la fenètre en gardant la possibilité de la rappeler
		launcher_mainframe.visible = true
		launcher_mainframe.size = launcher_mainframe.preferredSize
		launcher_mainframe.centerOnScreen
		//En l'absence de cette séquence bizarre, la MatteBorder du GridBagPanel de Launcher_Content n'est parfois pas affichée juste après le "run" de sbt
	}

	val main_character = Larissa
	main_character.visible = true
	main_character.say_smth(Array("Bienvenue dans mon monde !","J'ai hâte de commencer à jouer !","Alors ?<br>A quoi va-t-on jouer aujourd'hui ?", "Enchantée,<br>Moi, c'est Larissa", "Tu pourrais me coder un Advance Wars ?<br> S'te plait ! S'te plait ! S'te plait !"))
}

class Launcher_Content extends GridBagPanel { 
	opaque = true
	//INUTILE, mais conservé pour références futures
	//border = Swing.MatteBorder(5, 5, 5, 5, GUI_Mood.b_colour)	//MatteBorder permet de spécifier la largeur de la bordure sur chaque coté (haut, bas, gauche, droite)
	border = Swing.LineBorder(GUI_Mood.b_colour, 5)
	background = GUI_Mood.b_colour

	val launcher_return_messages = Array("Changeons de jeu !<br>A moins que tu n'aies envie de t'arrêter ?", "Envie de changer de jeu ?","Tu ne vas pas t'arrêter maintenant quand même ?")

	def constraints(x: Int, y: Int, 
			gridwidth: Int = 1, gridheight: Int = 1,
			weightx: Double = 0.0, weighty: Double = 0.0,
			fill: GridBagPanel.Fill.Value = GridBagPanel.Fill.None) 
	: Constraints = {
		val c = new Constraints
		c.gridx = x
		c.gridy = y
		c.gridwidth = gridwidth
		c.gridheight = gridheight
		c.weightx = weightx
		c.weighty = weighty
		c.fill = fill
		c
	}

	val title_label = new Label("Bienvenue,  joueur !"){
		font = new Font("Impact", 0, 35) // 0 pour normal, 1 pour gras, 2 pour italique ...
		foreground = GUI_Mood.f_colour
	}

	//Définition des caractèristiques de tailles des game_buttons
	val width = 160
	val height = 160
	val inner_border_thickness = 2
	val outer_border_thickness = 4
	val b_th = inner_border_thickness + outer_border_thickness 	//border_thickness

	//Définition des méthodes de peinture custom pour les game_buttons
	val lighter_dark_orchid = new Color(193,90,244)
	val darker_dark_orchid = new Color(123,20,174)
	val unoverflyed_background_gradientpaint = new java.awt.GradientPaint(b_th, b_th, lighter_dark_orchid, 0, height-2*b_th, darker_dark_orchid)
	val overflyed_background_gradientpaint = new java.awt.GradientPaint(b_th, b_th, darker_dark_orchid, 0, height-2*b_th, lighter_dark_orchid)

	def unoverflyed_custom_painting (g:Graphics2D) ={
		//définit un dégradé (x_origin, y_origin, origin_colour, x_destination, y_destination, destination_colour)
		g.setPaint(unoverflyed_background_gradientpaint)
		g.fillRect(b_th, b_th, width-2*b_th, height-2*b_th)				
	}
	def overflyed_custom_painting (g:Graphics2D) ={
		g.setPaint(overflyed_background_gradientpaint)
		g.fillRect(b_th, b_th, width-2*b_th, height-2*b_th)	
	}

	class Game_Button extends Button {
		this.peer.setOpaque(false);				//Ces deux lignes disent à la méthode paint de ne pas changer le background
		this.peer.setContentAreaFilled(false);
		var custom_painting: ((Graphics2D) => Unit) = ((g:Graphics2D) => ())
		override def paint(g: Graphics2D)={
			custom_painting(g)
			super.paint(g)
		}
		custom_painting = unoverflyed_custom_painting
		foreground = GUI_GE.silver
		border = Swing.LineBorder(GUI_Mood.f_colour, 4)
		border = Swing.CompoundBorder(outside = Swing.LineBorder(GUI_Mood.b_colour, outer_border_thickness)
										,inside = Swing.LineBorder(GUI_Mood.f_colour, inner_border_thickness))
		font = new Font("Impact", 0, 20)
		minimumSize = new Dimension(width, height)
		preferredSize = new Dimension(width, height)
		listenTo(mouse.moves, mouse.clicks)
		def mouse_enter_reaction () ={	//Lorsque la souris entre dans la zone du label
			this.custom_painting = overflyed_custom_painting
		}
		def mouse_exit_reaction () ={	//Lorsque la souris quitte la zone du label
			this.custom_painting = unoverflyed_custom_painting
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

	var game_button_list: IndexedSeq[Game_Button] = IndexedSeq()
	val nb_of_games = Main.game_list.length
	val nb_of_cols = (ceil(sqrt(nb_of_games))).toInt	//Calcul le nombre de colonnes dans le lanceur
	var row_number = 1	//La ligne et la colonne du gridbagpanel auxquelles on va ajouter le bouton de jeu
	var col_number = 0
	for (i <- 0 until nb_of_games) {	//Ajoute chacun des jeux de Main.game_list au GridBagPanel

		//var game_class = Class.forName(Main.game_list(i) + "$").getField("MODULE$").get(classOf[Game]).asInstanceOf[Game]

		/* */var game_class = Main.game_list(i)/* */
		def game_button_action() :Unit = {
			if(game_class.development_finished == false){Main.main_character.say("Ce jeu n'est pas disponible pour le moment. Le développeur n'a pas fini son travail.")}
			else{
				val game_frame = new UI(game_class)
				Main.launcher_mainframe.listenTo(game_frame)
				Main.launcher_mainframe.reactions += {
					case e: WindowClosing => {
						Main.launcher_mainframe.visible = true
						Main.launcher_mainframe.size = Main.launcher_mainframe.preferredSize
						Main.main_character.say_smth(launcher_return_messages)
					}
				}
				game_frame.visible = true
				game_frame.location = Main.launcher_mainframe.location
				Main.launcher_mainframe.dispose()
				reinit_game_button_list_custompainting()	//Sert à ce que le bouton cliqué reprenne l'apparence d'un bouton non survolé
			}
		}
		var game_button = new Game_Button{
			text = game_class.title
			action = Action(game_class.title)(game_button_action)
		}
		add(game_button,
			constraints(col_number, row_number))
		game_button_list = game_button_list :+ game_button
		col_number = col_number + 1
		if (col_number == nb_of_cols) {
			col_number = 0
			row_number = row_number + 1
		}
	}
	add(title_label,
		constraints(0,0,gridwidth = nb_of_cols, fill = GridBagPanel.Fill.Horizontal))
	Main.launcher_mainframe.resizable = false

	def reinit_game_button_list_custompainting () ={
		game_button_list.foreach{game_button =>
			game_button.custom_painting = unoverflyed_custom_painting
		}
	}
}
