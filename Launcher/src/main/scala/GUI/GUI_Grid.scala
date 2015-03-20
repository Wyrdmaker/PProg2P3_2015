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
	var state: String =""
}

//Est ce qu'on pourrait se défaire du paramètrage de Grid avec Game_Label_Class en allant chercher le type Game_Label_Class de game ??

//Crée un GridPanel d'une taille correspondant aux paramètres du jeu, puis le remplit avec des labels de la classe passée en argument.
// Fournit aussi 3 fonctions pour accéder aux labels de la grille ainsi créée
class Grid[Game_Label_Class <: Grid_Label] (game: Game) extends GridPanel(game.numeric_game_parameters_def_list(1)._2/* + 2*/, game.numeric_game_parameters_def_list(0)._2 /*+ 2*/) /*GridPanel prend le nb de lignes puis le nb de colonnes de la grille*/{
	val nb_of_cols = game.numeric_game_parameters_def_list(0)._2
	val nb_of_rows = game.numeric_game_parameters_def_list(1)._2

	/*
	//Remplir la première ligne de labels blancs qui pourront etre utilisés pour en faire des labels de bords
	for (cx <- 0 to nb_of_cols +1){
		contents += {new Label() }
	}
	*/
	//Remplir la grille d'objets de la classe Game_Label_Class, en créent des labels de bords à gauche et à droite de chaque ligne
	for (cy<-1 to nb_of_rows) {
		//contents += {new Label() }
		for (cx<- 1 to nb_of_cols) {
			val label = game.glb_factory()
			label.x = cx-1; label.y = cy-1; label.numero = (cy-1)*nb_of_cols +(cx-1);
			//Les labels sont numérotés de gauche à droite puis de haut en bas. La numérotation commence à 0 en haut à gauche de la grille
			contents += {label}
		}
		//contents += {new Label }
	}
	/*
	//Remplir la dernière ligne de labels blancs qui pourront etre utilisés pour en faire des labels de bords
	for (cx <- 0 to nb_of_cols +1){
		contents += {new Label() }
	}
	*/
	minimumSize = new Dimension(game.square_size_x * game.numeric_game_parameters_def_list(0)._2, game.square_size_y * game.numeric_game_parameters_def_list(1)._2 )

	/*//Test
	revalidate()
	repaint()*/
	
	//Renvoit le label de la case (x,y) (x et y commencent à 0)
	def access_xy(x: Int, y: Int) /*: Game_Label_Class*/={
		/*if (0 <= y*nb_of_cols + x && y*nb_of_cols + x <= nb_of_cols*nb_of_rows){
			contents(y*nb_of_cols + x + nb_of_cols+3 + y*2).asInstanceOf[Game_Label_Class]			
		}
		else{println("Anormal: on a donné à access_xy de coordonnées n'étant pas dans la grille")}
		game.glb_factory().asInstanceOf[Game_Label_Class]	//Juste pour satisfaire le compilateur*/
		contents(y*nb_of_cols + x).asInstanceOf[Game_Label_Class]

	}
	//Renvoit le label de numéro n
	def access_n(n: Int) ={
		//contents(n + nb_of_cols+3 + 2*(n/nb_of_cols)).asInstanceOf[Game_Label_Class]
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
abstract class Border_Grid_Orientation
case class Border_Grid_Horizontal extends Border_Grid_Orientation
case class Border_Grid_Vertical extends Border_Grid_Orientation

/*
def get_grid_border(length:Int, lb_factory: (() => Label_Class), orientation: Border_Grid_Orientation, square_size_x:Int, square_size_y:Int) : Grid_Border ={
	var return_value : Grid_Border =null
	orientation match {
		case Border_Grid_Horizontal => return_value = new Grid_Border(length, lb_factory, square_size_x, square_size_y, length, 1)
		case Border_Grid_Vertical => return_value = new Grid_Border(length, lb_factory, square_size_x, square_size_y, 1, length)
	}
	return(return_value)
}
*/

class Border_Grid[+Label_Class <: Label] (length:Int, lb_factory: (() => Label_Class), orientation: Border_Grid_Orientation, square_size_x:Int, square_size_y:Int) extends GridPanel(1, 1){
	//Une grille linéaire destinée à etre accolée à la grille de jeu
	orientation match {
		case Border_Grid_Horizontal() => {rows = 1; columns = length}
		case Border_Grid_Vertical() => {rows = length; columns = 1}
	}	
	for (c <- 0 until length){
		contents += {lb_factory()}
	}
	minimumSize = new Dimension(square_size_x * length, square_size_y)

	/*//Test
	revalidate()
	repaint()*/
	
	//Renvoit le label de numéro n
	def access_n(n: Int) ={
		//contents(n + nb_of_cols+3 + 2*(n/nb_of_cols)).asInstanceOf[Game_Label_Class]
		contents(n).asInstanceOf[Label_Class]
	}
	//Renvoit la liste des labels de la grille
	def get_contents() = {
		contents.map((x) => x.asInstanceOf[Label_Class])
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