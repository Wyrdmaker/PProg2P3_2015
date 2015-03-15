import scala.swing._
import scala.math._
import scala.swing.event._
import javax.swing.{ImageIcon, Icon}
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.border
import java.awt.GradientPaint;

object Main {
	val game_list: IndexedSeq[Game] = IndexedSeq( Demineur, Flip, Flip, Demineur)
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

	val nb_of_games = Main.game_list.length
	val nb_of_cols = (ceil(sqrt(nb_of_games))).toInt	//Calcul le nombre de colonnes dans le lanceur
	var row_number = 1	//La ligne et la colonne du gridbagpanel auxquelles on va ajouter le bouton de jeu
	var col_number = 0
	for (i <- 0 until nb_of_games) {	//Ajoute chacun des jeux de Main.game_list au GridBagPanel

		def game_button_action() :Unit = {
			val game_frame = new UI(Main.game_list(i))
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
		}
		var game_button = new Button(Main.game_list(i).title){
			//background = GUI_GE.dark_orchid
			foreground = GUI_GE.silver
			border = Swing.MatteBorder(4, 4, 4, 4, GUI_Mood.f_colour)
			//INUTILE, mais conservé pour référence future
			/*border = javax.swing.BorderFactory.createEtchedBorder(javax.swing.border.EtchedBorder.RAISED, GUI_GE.dark_golden_rod1, GUI_GE.dark_orchid)
			border = javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED, GUI_GE.dark_golden_rod1, GUI_GE.yellow)
			border = javax.swing.BorderFactory.createCompoundBorder(outside = Swing.LineBorder(GUI_GE.maroon4, 2), inside = Swing.LineBorder(GUI_GE.dark_golden_rod1, 2))*/
			val inner_border_thickness = 2
			val outer_border_thickness = 4
			border = Swing.CompoundBorder(outside = Swing.LineBorder(GUI_Mood.b_colour, outer_border_thickness)
											,inside = Swing.LineBorder(GUI_Mood.f_colour, inner_border_thickness))
			font = new Font("Impact", 0, 20)
			action = Action(Main.game_list(i).title)(game_button_action)
			val width = 160
			val height = 160
			val b_th = inner_border_thickness + outer_border_thickness 	//border_thickness
			minimumSize = new Dimension(width, height)
			preferredSize = new Dimension(width, height)
			val lighter_dark_orchid = new Color(193,90,244)
			val darker_dark_orchid = new Color(123,20,174)
			val background_gradientpaint = new java.awt.GradientPaint(b_th, b_th, lighter_dark_orchid, 0, height-2*b_th, darker_dark_orchid)
			//définit un dégradé (x_origin, y_origin, origin_colour, x_destination, y_destination, destination_colour)
			val darker_silver= new Color(180,182,200)
			//val foreground_gradientpaint = new java.awt.GradientPaint(0, 0, GUI_GE.silver, 0, 10, darker_silver) //Inutile mais conservé
			this.peer.setOpaque(false);				//Ces deux lignes disent à la méthode paint de ne pas changer le background
			this.peer.setContentAreaFilled(false);
			override def paint(g: Graphics2D)={
				g.setPaint(background_gradientpaint)
				g.fillRect(b_th, b_th, width-2*b_th, height-2*b_th)
				super.paint(g)
			}
		}
		add(game_button,
			constraints(col_number, row_number))
		col_number = col_number + 1
		if (col_number == nb_of_cols) {
			col_number = 0
			row_number = row_number + 1
		}
	}
	add(title_label,
		constraints(0,0,gridwidth = nb_of_cols, fill = GridBagPanel.Fill.Horizontal))
	Main.launcher_mainframe.resizable = false
}