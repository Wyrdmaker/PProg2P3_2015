import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.{ImageIcon, Icon}

package GUI{

abstract class Grid_Label extends Interactive_Label{
	var x = 0
	var y = 0
	var numero = 0
	var state: String
}

//Est ce qu'on pourrait se défaire du paramètrage de Grid avec Game_Label_Class en allant chercher le type Game_Label_Class de game ??

//Crée un GridPanel d'une taille correspondant aux paramètres du jeu, puis le remplit avec des labels de la classe passée en argument.
// Fournit aussi 3 fonctions pour accéder aux labels de la grille ainsi créée
class Grid[Game_Label_Class <: Grid_Label] (game: Game) extends GridPanel(game.numeric_game_parameters_def_list(1)._2, game.numeric_game_parameters_def_list(0)._2) /*GridPanel prend le nb de lignes puis le nb de colonnes de la grille*/{
	val nb_of_cols = game.numeric_game_parameters_def_list(0)._2
	val nb_of_rows = game.numeric_game_parameters_def_list(1)._2
	//Remplir la grille d'objets de la classe Game_Label_Class
	for (cy<-1 to nb_of_rows) {
		for (cx<- 1 to nb_of_cols) {
			val label = game.glb_factory()
			label.x = cx-1; label.y = cy-1; label.numero = (cy-1)*nb_of_cols +(cx-1);
			//Les labels sont numérotés de gauche à droite puis de haut en bas. La numérotation commence à 0 en haut à gauche de la grille
			contents += {label}
		}
	}
	minimumSize = new Dimension(game.square_size_x * game.numeric_game_parameters_def_list(0)._2, game.square_size_y * game.numeric_game_parameters_def_list(1)._2 )

	/*//Test
	revalidate()
	repaint()*/
	
	//Renvoit le label de la case (x,y) (x et y commencent à 0)
	def access_xy(x: Int, y: Int) ={
		contents(y*nb_of_cols + x).asInstanceOf[Game_Label_Class]
	}
	//Renvoit le label de numéro n
	def access_n(n: Int) ={
		contents(n).asInstanceOf[Game_Label_Class]
	}
	//Renvoit la liste des labels de la grille
	def get_contents() = {
		contents.map((x) => x.asInstanceOf[Game_Label_Class])
	}

	background = GUI_Mood.b_colour

	//Permet au jeu de peindre sur le background du gridpanel (par exemple pour y mettre des images)
	var background_painting: (Graphics2D, UIElement)=>Unit = (g:Graphics2D, uie:UIElement)=>()
	override def paintComponent(g:Graphics2D){
		super.paintComponent(g)
		background_painting(g, this)
	}

	def set_image_background(img: java.awt.Image, left_margin: Int = 0, top_margin: Int = 0, right_margin: Int = 0, bottom_margin: Int=0)={
		//Façon pratique de définir une image de background pour le GridPanel
		val old_background_painting = background_painting
		def new_background_painting(g:Graphics2D, uie:UIElement)={
			old_background_painting(g,uie)
			g.drawImage(img, left_margin, top_margin, uie.size.width - right_margin, uie.size.height - bottom_margin, null)
		}
		background_painting = new_background_painting
	}

}

}	//Accolade fermante du package GUI