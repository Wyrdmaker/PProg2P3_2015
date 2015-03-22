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

import GUI._
import Games.AdvanceWars._

package Games{
package AdvanceWars{

//"AdWGE" -> "AdvanceWars_Graphical_Element"
object AdWGE extends GUI_Graphical_Elements{
	val grey = new Color(96,96,96)
	val light_grey = new Color(160,160,160)
	val label_color_white = light_grey
	val label_color_black = grey
}

class AdvanceWars_Help_Frame extends Frame {
	title = "Aide"
	contents = new Label("Advance Wars"){
		background = GUI_Mood.b_colour
		foreground = GUI_Mood.f_colour
		opaque = true
	}
	visible = true
}

class AdvanceWars_About_Frame extends Frame{
	title = "A Propos"
	contents = new Label("Interface Graphique par T.Dupriez et G.Hocquet"){
		background = GUI_Mood.b_colour
		foreground = GUI_Mood.f_colour
		opaque = true
	}
	visible = true
}

object AdvanceWars extends Game{
	val title = "Advance Wars"
	development_finished = false

	val square_size_x = 50
	val square_size_y = 50
	var game_beginning_time: Date = null
	//var in_game = false héritée de Game

	//##Game parameters##
	var numeric_game_parameters_def_list = IndexedSeq(("Largeur", 0, 3, 25), ("Hauteur", 0, 3, 25), ("Nombre de Starting Flips",0,3,25))
	var string_game_parameters_def_list = IndexedSeq(("Niveau", "Bois zéro (2J)", IndexedSeq("Bois zéro (2J)")), ("Argent de départ","7000",IndexedSeq("0","4000","7000","10000","16000")))
	def nb_of_rows = numeric_game_parameters_def_list(1)._2  //fait de nb_of_rows un alias de la valeur du paramètre Height (ne marche que pour la lecture)
	def nb_of_cols = numeric_game_parameters_def_list(0)._2  //fait de nb_of_cols un alias de la valeur du paramètre Width (ne marche que pour la lecture)
	def nb_of_starting_flips = numeric_game_parameters_def_list(2)._2
	//Conservé pour futurs références mais inutile dans le démineur
	/*def nb_of_bombs = game_parameter_1 //Ces deux fonctions font de nb_of_bombs un alias de la variable game_parameter_1
	def nb_of_bombs_=(newval: Int) { game_parameter_1 = newval }*/

	type Game_Label_Class = AdvanceWars_Label
	def glb_factory () = { new Game_Label_Class } // "glb" -> "Game_Label_Class"
	type Game_Border_Label_Class = Label
	def gblb_factory () : Game_Border_Label_Class = {new Game_Border_Label_Class }
	def about_frame_factory () = { new AdvanceWars_About_Frame }
	def help_frame_factory () = { new AdvanceWars_Help_Frame }

	//var random_gen héritée de Game
	//var game_frame_content héritée de Game
	main_character_text_on_launching = main_character_text_on_launching
	main_character_text_on_win = main_character_text_on_win

	val game_game_mode_list = IndexedSeq(
		Game_Mode(IndexedSeq(16,12),IndexedSeq("Bois zéro (2J)","7000"))
	)
	def custom_game_parameters_conditions (form_nb_fields_result: IndexedSeq[Int]) ={
		var return_value = "OK"
		return_value
				
	}

	abstract class Type_Unite
	case class TU_Inftr extends Type_Unite
	case class TU_Veh extends Type_Unite

	abstract class Dep_Type
	case class DT_Inftr extends Dep_Type
	case class DT_Veh_A extends Dep_Type
	case class DT_Chars extends Dep_Type

	abstract class Classe_Unite{
		val type_unite: Type_Unite
		val dep_type: Dep_Type
		val carburant_max: Int
		val munitions_max: Int
		val dep: Int
		val vision: Int
		val portee_min: Int
		val portee_max: Int
		val prix: Int
		val atk_contre_inftr: Int
		val atk_contre_veh: Int
	}

	def game_starter () = {
		AdvanceWars.nb_of_moves = 0
		AdvanceWars.maj_nb_of_moves(0)
		//game_frame_content.bottom_panel.background = AdWGE.bottom_panel_color_list(AdWGE.no_color_mode)
		nb_of_white_square = nb_of_rows * nb_of_cols
		board = List()
		//Représente le plateau d'une partie: Une matrice de couples (Couleur, Cases_voisines_sous_influence)
		//Convention: La matrice est un tableau ligne (x) de tableaux colonnes (y)
		//Couleur est un booleen: true->White, false->Black
		//Cases_voisines_sous_influences est une List de 8 booleen correspondant aux cases voisines et indiquant si ces cases doivent
		//etre retournée lorsque la case centrale est cliquée
		//Convention: Les cases sont comptées de gauche à droite et de haut en bas
		var infl_list : List[Boolean] = List()
		for (x <- 0 until nb_of_cols){
			var x_col: List[(Boolean, List[Boolean])] = List()
			for (y <- 0 until nb_of_rows){
				infl_list = List()

				infl_list = /*(x < (nb_of_cols - 1) && y < (nb_of_rows - 1))*/ false :: infl_list	//bottom right
				infl_list = (y < (nb_of_rows - 1)) :: infl_list										//bottom
				infl_list = /*(x > 0 && y < (nb_of_rows - 1))*/ false :: infl_list					//bottom left
				infl_list = (x < (nb_of_cols - 1)) :: infl_list										//right
				infl_list = (x > 0) :: infl_list													//left
				infl_list = /*(x < (nb_of_cols - 1) && y > 0)*/ false :: infl_list					//top right
				infl_list = (y > 0) :: infl_list													//top
				infl_list = /*(x > 0 && y > 0)*/ false :: infl_list									//top left

				var xy_case: (Boolean,List[Boolean]) = (true,infl_list)
				x_col = xy_case :: x_col
			}
			board = x_col.reverse :: board
		}
		board = board.reverse

		//board printer
		/*println(board.length)
		 for (i <- 0 until nb_of_cols) {
		 for ( j <- 0 until nb_of_rows) {
			println(board(i)(j));
		 }
		}*/

		//Applique des flips au board (autant que spécifié dans les paramètres du jeu) avant que le joueur puisse jouer
		while (nb_of_white_square == nb_of_rows * nb_of_cols) {
			for (i <- 1 to nb_of_starting_flips) {
				var random_x = random_gen.nextInt(nb_of_cols-1)
				var random_y = random_gen.nextInt(nb_of_rows-1)
				flip(random_x, random_y)
			}
		}
		AdvanceWars.initial_nb_of_white_square = AdvanceWars.nb_of_white_square
		AdvanceWars.nb_of_moves = 0	//Car les flips initiaux ne comptent pas dans les flips effectués par le joueur
		AdvanceWars.maj_nb_of_moves(0)
		AdvanceWars.initial_board = AdvanceWars.board

		//Initialise chaque label selon le board
		for (y <- 0 until nb_of_rows) {
			for (x <- 0 until nb_of_cols) {
				val case_colour = board(x)(y)._1
				val case_infl_list = board(x)(y)._2
				game_frame_content.grid.access_xy(x,y).init(case_colour, case_infl_list)
			}
		}

	}
	def game_action_restart() : Unit = {
		AdvanceWars.nb_of_moves = 0
		AdvanceWars.maj_nb_of_moves(0)
		AdvanceWars.nb_of_white_square = initial_nb_of_white_square
		AdvanceWars.board = AdvanceWars.initial_board
		//Initialise chaque label selon le board
		for (y <- 0 until nb_of_rows) {
			for (x <- 0 until nb_of_cols) {
				val case_colour = board(x)(y)._1
				val case_infl_list = board(x)(y)._2
				game_frame_content.grid.access_xy(x,y).init(case_colour, case_infl_list)
			}
					
		}


	}
	//Définit ce qui se passe en cas de victoire du joueur -> voir Game
	override def win() = {
		super.win()		
	}
	//Définit ce qui se passe en cas de défaite du joueur -> voir Game
	override def lose() = {
		super.lose()
	}

	//##AdvanceWars Variables## // Variables internes au AdvanceWars
	var board: List[List[(Boolean, List[Boolean])]] = List()
	var initial_board: List[List[(Boolean, List[Boolean])]] = List()
	var nb_of_white_square = 0
	var initial_nb_of_white_square = 0
	var nb_of_moves = 0
	//##AdvanceWars Functions## //Fonctions internes au AdvanceWars
	//turn change la couleur de la case (x,y) dans board
	def turn (x: Int, y: Int) = {
		//println("a_turn called with: " + x + ", " + y)
		val previous_color = board(x)(y)._1
		if (previous_color) {	//la case était blanche
			//board(x)(y)._1 = false
			board = board.updated(x, board(x).updated(y, (false, board(x)(y)._2)))
			game_frame_content.grid.access_xy(x,y).turn()
			nb_of_white_square = nb_of_white_square - 1
		}
		else {					//la case était noire
			//board(x)(y)._1 = true
			board = board.updated(x, board(x).updated(y, (true, board(x)(y)._2)))
			nb_of_white_square = nb_of_white_square + 1
			game_frame_content.grid.access_xy(x,y).turn()
		}
	}
	//Renvoie les coordonnées de la case numéro i dans la liste d'influence de la case (x_base, y_base)
	def neighbour_square_xy (i: Int, x_base: Int, y_base: Int) ={
		var x_result = x_base
		var y_result = y_base
		def xpp () ={x_result = x_result + 1}
		def xmm () ={x_result = x_result - 1}
		def ypp () ={y_result = y_result + 1}
		def ymm () ={y_result = y_result - 1}
		i match {
			case 0 => {xmm; ymm}
			case 1 => {ymm}
			case 2 => {xpp; ymm}
			case 3 => {xmm}
			case 4 => {xpp}
			case 5 => {xmm; ypp}
			case 6 => {ypp}
			case 7 => {xpp; ypp}
		}
		(x_result, y_result)
	}
	//flip applique le résultat d'un clic sur la case (x,y) de board
	def flip (x: Int, y: Int) ={
		//println("a_flip called with: " + x + ", " + y)
		//println("infl_list: " + board(x)(y)._2)
		val infl_list = board(x)(y)._2
		for (i <- 0 to 7) {
			if (infl_list(i) == true) {
				val square_to_turn = neighbour_square_xy(i, x, y)
				turn(square_to_turn._1, square_to_turn._2)
			}
		}
		maj_nb_of_moves(1)
		turn(x,y)
		check_win()
	}

	def maj_nb_of_moves(n : Int /*normalement 1 ou 0*/) = {
		n match {
			case 1 => nb_of_moves = nb_of_moves + n 
			case 0 => nb_of_moves = nb_of_moves + n
			case _ => println("anormal: la fonction maj_nb_of_moves de l'objet AdvanceWars a été appelée avec un argument différent de 1 ou 0:" + n)
		}
		val label_1 = game_frame_content.label_1
		label_1.text = "Retournements : " + nb_of_moves.toString
	}

	def check_win () ={
		if (nb_of_white_square == nb_of_rows * nb_of_cols && playing) {
			AdvanceWars.win()
		}
	}
}

/*
object Main {
	def main(args: Array[String]) {
		val ui = new UI(AdvanceWars)
		ui.visible = true
	}
}*/


}	//accolade fermante du package AdvanceWars
}	//accolade fermante du package Games