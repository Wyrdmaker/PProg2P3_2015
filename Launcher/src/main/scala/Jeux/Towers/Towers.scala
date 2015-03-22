import scala.swing._
import scala.swing.event._
//import scala.swing.BorderPanel.Position._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat
import scala.math._
//import java.awt.event.{ActionEvent, ActionListener}
//import javax.swing.{ImageIcon, Icon}

import Games.Towers._
import GUI._

package Games{
package Towers{

//"TGE" -> "Towers_Graphical_Element"
object TGE extends GUI_Graphical_Elements{
	def no_color_mode () = {
		//Le max est une sécurité. Si IndexOf ne trouve pas la chaine correspondant au mode de couleur dans la liste de ses valeurs possibles, il renvoie -1.
		//Ainsi, en cas de faute de frappe, le mode de couleur utilisé est le Normal
		max(0,Towers.string_game_parameters_def_list(1)._3.indexOf(Towers.string_game_parameters_def_list(1)._2))
	}

	def label_color_unexplored () = {
		label_color_unexplored_list(no_color_mode())
	}
	def label_color_explored () ={
		label_color_explored_list(no_color_mode)
	}
	def label_color_flagged () ={
		label_color_flagged_list(no_color_mode)
	}

	def highlighted_border () ={
		highlighted_border_list(no_color_mode)
	}

	val label_color_unexplored_list = IndexedSeq(new Color(255,100,0), new Color(255, 0, 255), green, new Color(205,51,51), new Color(0,0,190))
	val label_color_explored_list = IndexedSeq(new Color(255,200,100), new Color(65,65,65), red, new Color(139,69,19), new Color(30, 144, 255))
	val label_color_flagged_list = IndexedSeq(new Color(255,50,50), cyan, blue, new Color(255,127,0), /*new Color(3, 180, 204)*/ yellow)
	val highlighted_border_list = IndexedSeq(border(blue,2), border(blue,2), border(blue,2), border(tan1,2), border(cyan,2))

	//val bottom_panel_color_list = IndexedSeq(white, cyan, white, tan1, dodger_blue)

	val towers_color_list = List (
	white,
	blue,
	green,
	red,
	cyan,
	purple,
	light_green,
	light_brown
)
}

class Towers_Help_Frame extends Frame {
	title = "Aide"
	contents = new Label("Allez, avouez que vous savez déjà jouer à ce jeu ! ;)"){
		background = GUI_Mood.b_colour
		foreground = GUI_Mood.f_colour
		opaque = true
	}
	visible = true
}

class Towers_About_Frame extends Frame{
	title = "A Propos"
	contents = new Label("Interface Graphique par T.Dupriez et G.Hocquet"){
		background = GUI_Mood.b_colour
		foreground = GUI_Mood.f_colour
		opaque = true
	}
	visible = true
}

object Towers extends Game{
	val title = "Towers"

	val square_size_x = 50
	val square_size_y = 50
	var game_beginning_time: Date = null
	//var in_game = false héritée de Game

	//##Game parameters##
	var numeric_game_parameters_def_list = IndexedSeq(("Largeur", 0, 4, 25), ("Hauteur", 0, 4, 25))
	var string_game_parameters_def_list = IndexedSeq(("Difficulté", "Facile", IndexedSeq("Facile", "Moyenne", "Difficile", "Absurde")), ("Mode de Couleur", "Classique", IndexedSeq("Classique", "Creepy-Glauque", "RVB", "Automne", "Océan")))
	def nb_of_rows = numeric_game_parameters_def_list(1)._2  //fait de nb_of_rows un alias de la valeur du paramètre Height (ne marche que pour la lecture)
	def nb_of_cols = numeric_game_parameters_def_list(0)._2  //fait de nb_of_cols un alias de la valeur du paramètre Width (ne marche que pour la lecture)
	def color_parameter = string_game_parameters_def_list(1)._2
		
	//Conservé pour futurs références mais inutile dans le démineur
	/*def nb_of_bombs = game_parameter_1 //Ces deux fonctions font de nb_of_bombs un alias de la variable game_parameter_1
	def nb_of_bombs_=(newval: Int) { game_parameter_1 = newval }*/

	type Game_Label_Class = Towers_Label
	def glb_factory () = { new Game_Label_Class } // "glb" -> "Game_Label_Class"
	type Game_Border_Label_Class = Towers_Label
	def gblb_factory () = {new Game_Border_Label_Class}
	def about_frame_factory () = { new Towers_About_Frame }
	def help_frame_factory () = { new Towers_Help_Frame }
        var border_labels : Array[Seq[Game_Border_Label_Class]] = Array()

	//var random_gen héritée de Game
	//var game_frame_content héritée de Game

	val game_game_mode_list = IndexedSeq(
		Game_Mode(IndexedSeq(4, 4),IndexedSeq("Facile", "Classique")),
		Game_Mode(IndexedSeq(5, 5),IndexedSeq("Moyenne", "Classique")),
		Game_Mode(IndexedSeq(6, 6),IndexedSeq("Difficile", "Classique"))	
	)
	def custom_game_parameters_conditions (form_nb_fields_result: IndexedSeq[Int]) ={ //form_nb_fields_result(0) = nb_of_cols, form_nb_fields_result(1) = nb_of_rows, form_nb_fields_result(2) = nb_of_bombs
		//val return_value = form_nb_fields_result(1) * form_nb_fields_result(0) > 9 && form_nb_fields_result(2) + 9 <= form_nb_fields_result(1) * form_nb_fields_result(0)
		var return_value = "OK"
		if (form_nb_fields_result(1) * form_nb_fields_result(0) <= 9) 
			return_value = "Grille trop petite"
		if (form_nb_fields_result(2) + 9 > form_nb_fields_result(1) * form_nb_fields_result(0))
			return_value = "Pas assez de place dans la grille pour les mines"
		return_value
				
	}	

	def game_starter () = {
		border_labels = Array()
		game_frame_content.set_right_border_grid()
		game_frame_content.set_bottom_border_grid()
		game_frame_content.set_left_border_grid()
		game_frame_content.set_top_border_grid()
                border_labels = border_labels :+ game_frame_content.top_border_grid.get_contents()
                border_labels = border_labels :+ game_frame_content.right_border_grid.get_contents()
                border_labels = border_labels :+ game_frame_content.bottom_border_grid.get_contents()
                border_labels = border_labels :+ game_frame_content.left_border_grid.get_contents()
                game_frame_content.set_bottom_right_border_label()
		game_frame_content.bottom_right_border_label.init(0,0)
		game_frame_content.bottom_right_border_label.text = ""
		game_frame_content.set_bottom_left_border_label()
                game_frame_content.bottom_left_border_label.init(0,0)
		game_frame_content.bottom_left_border_label.text = ""
		game_frame_content.set_top_right_border_label()
                game_frame_content.top_right_border_label.init(0,0)
		game_frame_content.top_right_border_label.text = ""
		game_frame_content.set_top_left_border_label()
                game_frame_content.top_left_border_label.init(0,0)
		game_frame_content.top_left_border_label.text = ""

		Towers.init_map();
		Towers.launch_game_timer()
		game_frame_content.grid.get_contents.foreach(label => label.init(1))

	}
	def game_action_restart() : Unit = {
		Towers.game_frame_content.grid.get_contents.foreach(label => label.init(1))
		Towers.launch_game_timer()
	}
	//Définit ce qui se passe en cas de victoire du joueur -> voir Game
	override def win() = {
		super.win()		
	}
	//Définit ce qui se passe en cas de défaite du joueur -> voir Game
	override def lose() = {
		super.lose()
	}

	//##Towers Variables## // Variables internes au Démineur

	//##Towers Functions## //Fonctions internes au Towers

	//A un numéro de case, associe la liste des numéros des cases adjacentes(en faisant attention aux bords de la grille)
/*
	def neighbour(n : Int) : List[Int] = {
		var lst : List[Int]= List()
		var a = if (n % nb_of_cols == 0) 0 else -1 //bord gauche du carré
		var b = if (n % nb_of_cols == nb_of_cols - 1) 0 else 1 //bord droit du carré
		var c = if (n < nb_of_cols) 0 else -1 // bord haut du carré
		var d = if (n >= (nb_of_rows - 1) * nb_of_cols) 0 else 1 // bord bas du carré
		for (i <- a to b) {
			for (j <- c to d) {
				if (0 <= n + j * nb_of_cols + i && n + j * nb_of_cols + i < nb_of_rows * nb_of_cols) {
					lst ++= List(n + j * nb_of_cols + i) // LOLILOOOL
				}
			}
		}
		return lst	
	}
*/

	//Est appelée lors du premier clic sur un label.
	//Place les bombes parmi les labels de la grille (autre que le label cliqué et ses 8 voisins).
	//Indique ensuite à chaque label (autre que ceux contenant une bombe) le nombre de ses voisins contenant une bombe -> label.value
	
	def suppress_value(v : Int, cur_x : Int, cur_y : Int) = {
		val grid = game_frame_content.grid
		for (x <- 0 until nb_of_cols) {
			var cur_label = grid.access_xy(x, cur_y)
			if (cur_label.value_t(v) && !cur_label.assigned) {
                		cur_label.value_t(v) = false
				cur_label.nb_val += 1
				if (cur_label.nb_val == nb_of_rows - 1)
					cur_label.affect()
			}
		}
		for (y <- 0 until nb_of_rows) {
			var cur_label = grid.access_xy(cur_x, y)
                        if (cur_label.value_t(v) && !cur_label.assigned) {
                                cur_label.value_t(v) = false
				cur_label.nb_val += 1
				if (cur_label.nb_val == nb_of_rows - 1)
                                        cur_label.affect()
                        }
		}
	}
	
	def init_map() = {
		val grid = game_frame_content.grid
		var label_vide = true
		while (label_vide) {
			var random_x = random_gen.nextInt(nb_of_cols)
			var random_y = random_gen.nextInt(nb_of_rows)
			var cur_label = grid.access_xy(random_x, random_y)
			if (!cur_label.assigned) {
				cur_label.affect()
				label_vide = false
				for (n <- 0 until nb_of_rows * nb_of_cols)
					if (!grid.access_n(n).assigned)
						label_vide = true
			}
		}

		def nb_valeurs_visibles(b: Int, n: Int): Int = {
			var mini = 0
			var r = 0
			var t = Array((0, 1), (1, 0), (0, 1), (1, 0))
			var d = if (b == 1 || b == 2) nb_of_rows - 1 else 0
			var f = if (b == 1 || b == 2) 0 else nb_of_rows -1
			var s = if (b == 1 || b == 2) -1 else 1
			for (i <- d to f by s)
				if (game_frame_content.grid.access_xy(t(b)._1 * i + t(b)._2 * n, t(b)._2 * i + t(b)._1 * n).value > mini) {
					mini = game_frame_content.grid.access_xy(t(b)._1 * i + t(b)._2 * n, t(b)._2 * i + t(b)._1 * n).value
					r += 1
				}
			r
		}
		
		//initialiser les labels de conditions de la grille
                for (n <- 0 until nb_of_rows)
			for (i <- 0 until 4)
                        	border_labels(i)(n).init(0, nb_valeurs_visibles(i, n))
	}
}

/*
object Main {
	def main(args: Array[String]) {
		val ui = new UI(Towers)
		ui.visible = true
	}
}*/

}

}
