import scala.swing._
import scala.swing.event._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat
import scala.math._

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
	val label_color_flagged_list = IndexedSeq(new Color(255,50,50), cyan, blue, new Color(255,127,0), yellow)
	val highlighted_border_list = IndexedSeq(border(blue,2), border(blue,2), border(blue,2), border(tan1,2), border(cyan,2))
}

class Towers_Help_Frame extends Frame {
	title = "Aide"
	contents = new Label(){
                background = GUI_Mood.b_colour
                foreground = GUI_Mood.f_colour
                opaque = true
                text = "<html> <body> <head style=\"font-size:25px; font-family:arial;\"> <u> Aide de Towers </u> </head> <br> <p style=\"font-size:13px; font-family:arial;\">" +
                "En cliquant sur les cases, faites apparaître des chiffres de sorte que : <br>" +
                "       - Chaque chiffre apparaisse exactement une fois pour chaque ligne et chaque colonne <br> " +
		"	- Les numéros sur les côtés indiquent la plus longue sous-suite croissante lue à partir de ces positions " +
                "</p> </body> </html>"
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

//##Game parameters##
        var numeric_game_parameters_def_list = IndexedSeq(("Largeur", 0, 2, 8), ("Hauteur", 0, 2, 8))
        var string_game_parameters_def_list = IndexedSeq(("Difficulté", "Facile", IndexedSeq("Facile", "Moyenne", "Difficile", "Absurde")), ("Mode de Couleur", "Classique", IndexedSeq("Classique", "Creepy-Glauque", "RVB", "Automne", "Océan")))
        def size = numeric_game_parameters_def_list(0)._2  //fait de nb_of_rows un alias de la valeur du paramètre Height (ne marche que pour la lecture)
        def color_parameter = string_game_parameters_def_list(1)._2

	type Game_Label_Class = Towers_Label
	def glb_factory () = { new Game_Label_Class } // "glb" -> "Game_Label_Class"
	type Game_Border_Label_Class = Towers_Label
	def gblb_factory () = {new Game_Border_Label_Class}
	def about_frame_factory () = { new Towers_About_Frame }
	def help_frame_factory () = { new Towers_Help_Frame }
        var border_labels : Array[Seq[Game_Border_Label_Class]] = Array()

	val game_game_mode_list = IndexedSeq(
		Game_Mode(IndexedSeq(4,4),IndexedSeq("Facile", "Classique")),
		Game_Mode(IndexedSeq(5,5),IndexedSeq("Moyenne", "Classique")),
		Game_Mode(IndexedSeq(6,6),IndexedSeq("Difficile", "Classique"))	
	)

	def custom_game_parameters_conditions (form_nb_fields_result: IndexedSeq[Int]) = {
		"OK"
	}	

	def game_starter () = {
		// Instancier les bordures
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
		game_frame_content.bottom_right_border_label.border_init(0)
		game_frame_content.set_bottom_left_border_label()
                game_frame_content.bottom_left_border_label.border_init(0)
		game_frame_content.set_top_right_border_label()
                game_frame_content.top_right_border_label.border_init(0)
		game_frame_content.set_top_left_border_label()
                game_frame_content.top_left_border_label.border_init(0)

		Towers.init_map();
		Towers.launch_game_timer()
		game_frame_content.grid.get_contents.foreach(label => label.init())

	}
	def game_action_restart() : Unit = {
		Towers.game_frame_content.grid.get_contents.foreach(label => label.init())
		Towers.grid_check()
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


	def suppress_value(v : Int, cur_x : Int, cur_y : Int) = {
		val grid = game_frame_content.grid
		for (x <- 0 until size) {
			var cur_label = grid.access_xy(x, cur_y)
			if (cur_label.value_t(v) && !cur_label.assigned) {
                		cur_label.value_t(v) = false
				cur_label.nb_val += 1
				if (cur_label.nb_val == size - 1)
					cur_label.affect()
			}
		}
		for (y <- 0 until size) {
			var cur_label = grid.access_xy(cur_x, y)
                        if (cur_label.value_t(v) && !cur_label.assigned) {
                                cur_label.value_t(v) = false
				cur_label.nb_val += 1
				if (cur_label.nb_val == size - 1)
                                        cur_label.affect()
                        }
		}
	}
	
	def init_map() = {
		val grid = game_frame_content.grid
		var label_vide = true
		while (label_vide) {
			var random_x = random_gen.nextInt(size)
			var random_y = random_gen.nextInt(size)
			var cur_label = grid.access_xy(random_x, random_y)
			if (!cur_label.assigned) {
				cur_label.affect()
				label_vide = false
				for (n <- 0 until size * size)
					if (!grid.access_n(n).assigned)
						label_vide = true
			}
		}

		def nb_valeurs_visibles(b: Int, n: Int): Int = {
			var mini = 0
			var r = 0
			var t = Array((0, 1), (1, 0), (0, 1), (1, 0))
			var d = if (b == 1 || b == 2) size - 1 else 0
			var f = if (b == 1 || b == 2) 0 else size -1
			var s = if (b == 1 || b == 2) -1 else 1
			for (i <- d to f by s)
				if (game_frame_content.grid.access_xy(t(b)._1 * i + t(b)._2 * n, t(b)._2 * i + t(b)._1 * n).value > mini) {
					mini = game_frame_content.grid.access_xy(t(b)._1 * i + t(b)._2 * n, t(b)._2 * i + t(b)._1 * n).value
					r += 1
				}
			r
		}
		
		//initialiser les labels de conditions de la grille
                for (n <- 0 until size)
			for (i <- 0 until 4)
                        	border_labels(i)(n).border_init(nb_valeurs_visibles(i, n))
	}
	
	// Recherche les incohérances
	def grid_check() = {
                var g = game_frame_content.grid
                var n = size
                var v = true
                g.get_contents.foreach(label => label.change_to_state(label, "1"))
                for (x <- 0 until n) {
                        var t: Array[Array[Int]] = Array.fill(n + 1){Array()}
                        for (y <- 0 until n)
                                t(g.access_xy(x, y).num) = t(g.access_xy(x, y).num):+ y
                        for (y <- 1 to n) {
                                if (t(y).length > 1) {
                                        v = false
                                        for (i <- 0 until t(y).length)
                                                g.access_xy(x, t(y)(i)).change_to_state(g.access_xy(x, t(y)(i)), "2")
                                }
                                if (t(y).length == 0)
                                        v = false
                        }
                }
                for (y <- 0 until n) {
                        var t: Array[Array[Int]] = Array.fill(n + 1){Array()}
                        for (x <- 0 until n)
                                t(g.access_xy(x, y).num) = t(g.access_xy(x, y).num):+ x
                        for (x <- 1 to n) {
                                if (t(x).length > 1) {
                                        v = false
                                        for (i <- 0 until t(x).length)
                                                g.access_xy(t(x)(i), y).change_to_state(g.access_xy(t(x)(i), y), "2")
                                        }
                                if (t(x).length == 0)
                                        v = false
                        }
                }
                def nb_num_visibles(b: Int, j: Int): Int = {
                        var mini = 0
                        var r = 0
                        var t = Array((0, 1), (1, 0), (0, 1), (1, 0))
                        var d = if (b == 1 || b == 2) n - 1 else 0
			var f = if (b == 1 || b == 2) 0 else n -1
                        var s = if (b == 1 || b == 2) -1 else 1
                        for (i <- d to f by s)
                                if (g.access_xy(t(b)._1 * i + t(b)._2 * j, t(b)._2 * i + t(b)._1 * j).num > mini) {
                                        mini = g.access_xy(t(b)._1 * i + t(b)._2 * j, t(b)._2 * i + t(b)._1 * j).num
                                        r += 1
                                }
                        r
                }
                for (i <- 0 until 4)
                        for (j <- 0 until n) {
                                var u = nb_num_visibles(i, j)
                                if (u > border_labels(i)(j).condition) {
                                        border_labels(i)(j).foreground = TGE.red
                                        v = false
                                }
                                else if (u == border_labels(i)(j).condition)
                                        border_labels(i)(j).foreground = TGE.green
                                else
                                        border_labels(i)(j).foreground = TGE.dark_golden_rod1
                        }
                if (v)
                        win()
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
