import scala.swing._
import scala.swing.event._
//import scala.swing.BorderPanel.Position._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat
import scala.math._
import collection.mutable.Buffer
import java.awt.event.{ActionEvent, ActionListener}
//import java.awt.event.{ActionEvent, ActionListener}
//import javax.swing.{ImageIcon, Icon}

import GUI._
import Games.Life._

package Games{
package Life{

//"LGE" -> "Life_Graphical_Element"
object LGE extends GUI_Graphical_Elements{
	val grey = new Color(96,96,96)
	val light_grey = new Color(160,160,160)

	def no_color_mode () = {
		//Le max est une sécurité. Si IndexOf ne trouve pas la chaine correspondant au mode de couleur dans la liste de ses valeurs possibles, il renvoie -1.
		//Ainsi, en cas de faute de frappe, le mode de couleur utilisé est le Normal
		max(0,Life.string_game_parameters_def_list(0)._3.indexOf(Life.string_game_parameters_def_list(0)._2))
	}

	def label_color_black () = {
		label_color_black_list(no_color_mode())
	}
	def label_color_white () ={
		label_color_white_list(no_color_mode)
	}

	val label_color_black_list = IndexedSeq(grey, violet_fluo, white)
	val label_color_white_list = IndexedSeq(white, cyan, grey)

	//val bottom_panel_color_list = IndexedSeq(white, grey)

}

class Life_Help_Frame extends Frame {
	title = "Aide"
	contents = new Label(){
		background = GUI_Mood.b_colour
		foreground = GUI_Mood.f_colour
		opaque = true
		text = "<html> <body> <head style=\"font-size:25px; font-family:arial;\"> <u> Aide de Life </u> </head> <br> <p style=\"font-size:13px; font-family:arial;\">  " +
		"L'univers de Life est une grille infini de cellules, chacune d'elles pouvant être dans un des deux états possibles, Vivante ou Morte.<br>" +
		"Chaque cellule interagit avec ses voisines, qui sont les cellules qui sont horizontalement, verticalement ou diagonalement adjacentes.<br>" +
		"A chaque étape de temps, les transformations suivantes se déroulent: <br>" +
		"		- Une cellule vivante avec moins de deux voisines en vie meurt (sous-population).<br>" +
		"		- Une cellule vivante avec deux ou trois voisines en vie reste en vie.<br>" +
		"		- Une cellule vivante avec plus de trois voisines en vie meurt (sur-population).<br>" +
		"		- Une cellule morte avec exactement trois voisines en vie revient à la vie (reproduction).<br>" +
		"<br>" +
		"Le bouton Évolution/Pause permet de lancer ou d'arrêter le processus d'évolution.<br>" +
		"Le bouton Sauvegarder permet de sauvegarder l'état de la grille. Vous pouvez revenir plus tard à cet état via le bouton Charger.<br>" +
		"Le bouton Lent/Normal/Rapide permet de configurer la vitesse d'évolution.<br>"+
		"Le bouton Recommencer du menu Jeu permet de nettoyer la grille.<br>"+
		"</p> </body> </html>"
	}
	visible = true
	resizable = false
	centerOnScreen()
}

class Life_About_Frame extends Frame{
	title = "A Propos"
	contents = new Label("Interface Graphique par T.Dupriez et G.Hocquet"){
		background = GUI_Mood.b_colour
		foreground = GUI_Mood.f_colour
		opaque = true
	}
	visible = true
}

object Life extends Game{
	val title = "Life"

	val square_size_x = 20
	val square_size_y = 20
	var game_beginning_time: Date = null
	timer_needed = false
	//var in_game = false héritée de Game

	//##Game parameters##
	var numeric_game_parameters_def_list = IndexedSeq(("Largeur", 0, 5, 50), ("Hauteur", 0, 6, 50))
	var string_game_parameters_def_list = IndexedSeq(("Mode de Couleur", "Classique", IndexedSeq("Classique")))
	def nb_of_rows = numeric_game_parameters_def_list(1)._2  //fait de nb_of_rows un alias de la valeur du paramètre Height (ne marche que pour la lecture)
	def nb_of_cols = numeric_game_parameters_def_list(0)._2  //fait de nb_of_cols un alias de la valeur du paramètre Width (ne marche que pour la lecture)
	def color_parameter = string_game_parameters_def_list(0)._2
	val has_numeric_parameters_0asWidth_1asHeight = true
		
	//Conservé pour futurs références mais inutile dans le démineur
	/*def nb_of_bombs = game_parameter_1 //Ces deux fonctions font de nb_of_bombs un alias de la variable game_parameter_1
	def nb_of_bombs_=(newval: Int) { game_parameter_1 = newval }*/

	type Game_Label_Class = Life_Label
	def glb_factory () = { new Game_Label_Class } // "glb" -> "Game_Label_Class"
	type Game_Border_Label_Class = Life_Border_Label
	def gblb_factory () : Game_Border_Label_Class = {new Game_Border_Label_Class }
	def about_frame_factory () = { new Life_About_Frame }
	def help_frame_factory () = { new Life_Help_Frame }

	//var random_gen héritée de Game
	//var game_frame_content héritée de Game
	main_character_text_on_launching = main_character_text_on_launching ++ Array("Ah, la vie...<br>Un jeu intriguant.", "Si tu crée le motif:<br> <Pre> o <br>  o<br>ooo</Pre>Tu obtiendra une sorte de vaisseau qui voyagera.")
	//enabled_main_character_speak_on_long_play = false

	val game_game_mode_list = IndexedSeq(
		Game_Mode(IndexedSeq(10,10),IndexedSeq("Classique")),
		Game_Mode(IndexedSeq(25,25),IndexedSeq("Classique")),
		Game_Mode(IndexedSeq(50,50),IndexedSeq("Classique"))
	)
	def custom_game_parameters_conditions (form_nb_fields_result: IndexedSeq[Int]) ={
		var return_value = "OK"
		return_value
	}

	def int_is_alive(xy:(Int,Int))={	//Renvoie 1 si la case de coordonnée xy est en vie, 0 sinon
		if(board(xy._1)(xy._2)(0)){1}
		else{0}
	}

	def mod_x(x:Int) :Int={	//renvoie 0 si x vaut nb_of_cols, nb_of_cols si x vaut -1 et x sinon
		if(x==nb_of_cols){return(0)}
		if(x== -1){return(nb_of_cols-1)}
		return(x)
	}

	def mod_y(y:Int) :Int={	//renvoie 0 si y vaut nb_of_rows, nb_of_rows si y vaut -1 et y sinon
		if(y==nb_of_rows){return(0)}
		if(y== -1){return(nb_of_rows-1)}
		return(y)
	}

	def mod_xy(x:Int,y:Int)={	//renvoie le couple xy en ayant appliqué les fonctions mod_x et mod_y
		(mod_x(x),mod_y(y))
	}

	def nb_of_living_neighbours(x:Int,y:Int): Int={	//renvoie le nombre de cases vivantes adjacentes à la case (x,y)
		var result = 0
		result = result + int_is_alive(mod_xy(x-1,y-1))	//top left
		result = result + int_is_alive(mod_xy(x,y-1))	//top
		result = result + int_is_alive(mod_xy(x+1,y-1))	//top right

		result = result + int_is_alive(mod_xy(x-1,y))	//left
		result = result + int_is_alive(mod_xy(x+1,y))	//right

		result = result + int_is_alive(mod_xy(x-1,y+1))	//bottom left
		result = result + int_is_alive(mod_xy(x,y+1))	//bottom
		result = result + int_is_alive(mod_xy(x+1,y+1))	//bottom right
		return(result)
	}

	def change_cell_next_state(x:Int,y:Int,next_state:Boolean)={
		board(x)(y)(1)=next_state
		//if(next_state){game_frame_content.grid.access_xy(x,y).next_state = "alive"}
		//else{game_frame_content.grid.access_xy(x,y).next_state = "dead"}
	}

	def compute_next_state(x:Int,y:Int)={	//Calcule le nouvel état de la cellule (x,y)
		val nb_of_living_adjacent_cells = nb_of_living_neighbours(x,y)
		if(board(x)(y)(0) && nb_of_living_adjacent_cells < 2){board(x)(y)(1)=false/*; println("("+x+","+y+") "+"under-population")*/}	//under-population
		else{
			if(board(x)(y)(0) && 2 <= nb_of_living_adjacent_cells && nb_of_living_adjacent_cells <= 3){board(x)(y)(1)=true/*; println("("+x+","+y+") "+"stay alive")*/}
			else{
				if(board(x)(y)(0) && 3 < nb_of_living_adjacent_cells){board(x)(y)(1)=false/*; println("("+x+","+y+") "+"overcrowding")*/}	//overcrowding
				else{
					if(!(board(x)(y)(0)) && nb_of_living_adjacent_cells == 3){board(x)(y)(1)=true/*; println("("+x+","+y+") "+"reproduction")*/}	//reproduction
				}
			}
		}
	}

	def all_compute_next_state() ={	//Calcule le nouvel état de chaque cellule
		for (x <- 0 until nb_of_cols){
			for(y <- 0 until nb_of_rows){
				compute_next_state(x,y)
			}
		}
	}

	def all_evolve() ={	//Fait évoluer toutes les cellules dans leurs nouvel état
		for(n <- 0 until nb_of_rows*nb_of_cols){
			game_frame_content.grid.access_n(n).evolve()
		}
	}

	val evolution_timer_listener = new ActionListener{
		def actionPerformed(e: ActionEvent) {
			if(Life.running){
				all_compute_next_state()
				all_evolve()
			}
		}
	}

	var tick_delay = 1000
	var evolution_timer = new javax.swing.Timer(tick_delay, evolution_timer_listener)

	def change_tick_delay(new_delay:Int)={
		tick_delay=new_delay
		evolution_timer.stop()
		evolution_timer = new javax.swing.Timer(tick_delay, evolution_timer_listener)
		evolution_timer.start()
	}

	def build_empty_board() ={	//Construit board en le remplissant de cellules mortes
		board = Buffer()
		for (x <- 0 until nb_of_cols) {
			board += Buffer()
			for (y <- 0 until nb_of_rows) {
				board(x) += Buffer(false,false)
			}
		}	
	}

	def apply_board_to_the_grid()={
		for (x <- 0 until nb_of_cols) {
			for (y <- 0 until nb_of_rows) {
				game_frame_content.grid.access_xy(x,y).init(board(x)(y)(0))
			}
		}			
	}

	def game_starter () = {
		board = Buffer()

		game_frame_content.set_left_border_grid()
		go_stop_label = game_frame_content.left_border_grid.access_n(0)
		go_stop_label.init("go/stop_label")
		game_frame_content.left_border_grid.access_n(2).init("save_label")
		game_frame_content.left_border_grid.access_n(3).init("load_label")
		game_frame_content.left_border_grid.access_n(5).init("speed_label")
		//Remplit le board et initialise chaque label selon le board
		build_empty_board()
		apply_board_to_the_grid()
		make_saved_board_as_board()
		//saved_board = board.clone()	Alias les deux tableaux (pourquoi ?)
		evolution_timer.start()
	}

	def stop_evolution()={	//Met running à false et dit au label go/stop de refléter ce changement
		running = false
		go_stop_label.text="Évolution"

	}

	def make_board_as_saved_board() ={
		board = Buffer()
		for (x <- 0 until nb_of_cols) {
			board += Buffer()
			for (y <- 0 until nb_of_rows) {
				board(x) += Buffer(saved_board(x)(y)(0),saved_board(x)(y)(1))
			}
		}		
	}

	def make_saved_board_as_board() ={
		saved_board = Buffer()
		for (x <- 0 until nb_of_cols) {
			saved_board += Buffer()
			for (y <- 0 until nb_of_rows) {
				saved_board(x) += Buffer(board(x)(y)(0),board(x)(y)(1))
			}
		}		
	}

	def game_action_restart() : Unit = {
		stop_evolution()
		build_empty_board()
		apply_board_to_the_grid()
		//Initialise chaque label selon le board
		/*for (y <- 0 until nb_of_rows) {
			for (x <- 0 until nb_of_cols) {
				game_frame_content.grid.access_xy(x,y).init(board(x)(y)(0))
			}
					
		}*/


	}
	//Définit ce qui se passe en cas de victoire du joueur -> voir Game
	override def win() = {
		super.win()		
	}
	//Définit ce qui se passe en cas de défaite du joueur -> voir Game
	override def lose() = {
		super.lose()
	}

	//##Life Variables## // Variables internes au Life
	//board et saved_board sont des tableaux 2D de couples de booléens (enregistrés comme des Buffers). La première valeur du couple est
	//l'état actuel de la cellule (true => en vie, false => morte). La seconde valeur est l'état dans lequel sera la cellule après le prochain tick
	var board: Buffer[Buffer[Buffer[Boolean]]] = Buffer()
	var saved_board: Buffer[Buffer[Buffer[Boolean]]] = Buffer()
	var running:Boolean = false
	var go_stop_label: Game_Border_Label_Class = null
	//##Life Functions## //Fonctions internes au Life
}

}	//accolade fermante du package Life
}	//accolade fermante du package Games