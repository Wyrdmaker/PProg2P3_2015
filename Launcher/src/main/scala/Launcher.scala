import scala.swing._
import scala.math._
import scala.swing.event._
import javax.swing.{ImageIcon, Icon}
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.border
import java.awt.GradientPaint
import java.io._

object Main {
	var game_list = new File("src/main/scala/Jeux").listFiles.filter(_.isDirectory).map(_.getName)
	//Pour ajouter des jeux, ajouter dans le dossier Jeux un dossier contenant les fichiers <nom_du_jeu>.scala et
	// <nom_du_jeu>_Label_and_Label_States.scala. Le dossier ajouté doit porter le meme nom que le nom de la classe 
	//étendant la classe Game dans le jeu ajouté.
	var launcher_mainframe : MainFrame = null
	def main(args: Array[String]) {
		launcher_mainframe = new MainFrame{
			iconImage = toolkit.getImage("src/main/ressources/my_purple_dice_20.png")
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
		}
		launcher_mainframe.dispose()	//dispose() permet de renvoyer la fenètre en gardant la possibilité de la rappeler
		launcher_mainframe.centerOnScreen
		launcher_mainframe.visible = true
		launcher_mainframe.size = launcher_mainframe.preferredSize
		//En l'absence de cette séquence bizarre, la MatteBorder du GridBagPanel de Launcher_Content n'est pas affichée juste après le "run" de sbt
	}
}

class Launcher_Content extends GridBagPanel { 
	opaque = true
	border = Swing.MatteBorder(5, 5, 5, 5, GUI_Mood.b_colour)	//MatteBorder permet de spécifier la largeur de la bordure sur chaque coté (haut, bas, gauche, droite)
																//(Ici, on aurait juste pu mettre LineBorder)
	background = GUI_Mood.b_colour
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
		border = Swing.MatteBorder(4, 4, 4, 4, GUI_Mood.f_colour)
		//INUTILE, mais conservé pour référence future
		/*border = javax.swing.BorderFactory.createEtchedBorder(javax.swing.border.EtchedBorder.RAISED, GUI_GE.dark_golden_rod1, GUI_GE.dark_orchid)
		border = javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED, GUI_GE.dark_golden_rod1, GUI_GE.yellow)
		border = javax.swing.BorderFactory.createCompoundBorder(outside = Swing.LineBorder(GUI_GE.maroon4, 2), inside = Swing.LineBorder(GUI_GE.dark_golden_rod1, 2))*/
		border = Swing.CompoundBorder(outside = Swing.LineBorder(GUI_Mood.b_colour, outer_border_thickness)
										,inside = Swing.LineBorder(GUI_Mood.f_colour, inner_border_thickness))
		font = new Font("Impact", 0, 20)
		minimumSize = new Dimension(width, height)
		preferredSize = new Dimension(width, height)

		//val foreground_gradientpaint = new java.awt.GradientPaint(0, 0, GUI_GE.silver, 0, 10, darker_silver) //Inutile mais conservé
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

		var game_class = Class.forName(Main.game_list(i) + "$").getField("MODULE$").get(classOf[Game]).asInstanceOf[Game]
		def game_button_action() :Unit = {
			val game_frame = new UI(game_class)
			Main.launcher_mainframe.listenTo(game_frame)
			Main.launcher_mainframe.reactions += {
				case e: WindowClosing => {
					Main.launcher_mainframe.visible = true
					Main.launcher_mainframe.size = Main.launcher_mainframe.preferredSize
				}
			}
			game_frame.visible = true
			game_frame.location = Main.launcher_mainframe.location
			Main.launcher_mainframe.dispose()
			reinit_game_button_list_custompainting()	//Sert à ce que le bouton cliqué reprenne l'apparence d'un bouton non survolé
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
