import scala.swing._
import scala.math._
import scala.swing.event._
import javax.swing.{ImageIcon, Icon}
import java.awt.event.{ActionEvent, ActionListener}

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
			background = GUI_GE.dark_orchid
			foreground = GUI_Mood.f_colour
			border = Swing.MatteBorder(5, 5, 5, 5, GUI_Mood.b_colour)
			font = new Font("Impact", 0, 20)
			action = Action(Main.game_list(i).title)(game_button_action)
			minimumSize = new Dimension(160, 160)
			preferredSize = new Dimension(160, 160)
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