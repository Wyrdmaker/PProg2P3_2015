import scala.swing._
import scala.swing.event._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat
import scala.math._
import scala.Array._

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
	def normal_foreground_colour () ={
		normal_foreground_colour_list(no_color_mode)
	}

	def background_colour ()={
		background_colour_list(no_color_mode)
	}

	def highlighted_border () ={
		highlighted_border_list(no_color_mode)
	}

	val normal_foreground_colour_list = IndexedSeq(black, cyan, blue, new Color(255,127,0), new Color(30, 144, 255))
	val background_colour_list = IndexedSeq(new Color(255,100,0), new Color(255, 0, 255), green, new Color(205,51,51), new Color(0,0,190))
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
		var numeric_game_parameters_def_list = IndexedSeq(("Taille", 0, 2, 8))
		var string_game_parameters_def_list = IndexedSeq(("Difficulté", "Facile", IndexedSeq("Facile", "Difficile", "Libre")), ("Mode de Couleur", "Classique", IndexedSeq("Classique", "Creepy-Glauque", "RVB", "Automne", "Océan")),("Mode Spectateur", "Joueur", IndexedSeq("Joueur","Spectateur")))
		def size = numeric_game_parameters_def_list(0)._2  //fait de size un alias de la valeur du paramètre Taille (ne marche que pour la lecture)
		def nb_of_rows = size
		def nb_of_cols = size
		def color_parameter = string_game_parameters_def_list(1)._2
	val has_numeric_parameters_0asWidth_1asHeight = false

	type Game_Label_Class = Towers_Label
	def glb_factory () = { new Game_Label_Class } // "glb" -> "Game_Label_Class"
	type Game_Border_Label_Class = Towers_Label
	def gblb_factory () = {new Game_Border_Label_Class}
	def about_frame_factory () = { new Towers_About_Frame }
	def help_frame_factory () = { new Towers_Help_Frame }
	var border_labels: Array[Seq[Game_Border_Label_Class]] = Array()
	var motif_futur: Array[Array[Array[Int]]] = Array()

	val game_game_mode_list = IndexedSeq(
		Game_Mode(IndexedSeq(4),IndexedSeq("Facile", "Classique","Joueur")),
		Game_Mode(IndexedSeq(4),IndexedSeq("Difficile", "Classique","Joueur")),
		Game_Mode(IndexedSeq(5),IndexedSeq("Facile", "Classique","Joueur")),
		Game_Mode(IndexedSeq(5),IndexedSeq("Difficile", "Classique","Joueur")),
		Game_Mode(IndexedSeq(6),IndexedSeq("Libre", "Classique","Joueur"))	
	)

	def custom_game_parameters_conditions (form_nb_fields_result: IndexedSeq[Int]) = {
		"OK"
	}	

	def init_futur(deb: Boolean) = {
		var g = game_frame_content.grid
                motif_futur = Array.fill(size){Array.fill(size){Array.fill(Math.pow(size + 1,size + 1).toInt){0}}}
                var motif: Array[Int] = Array.fill(size){1}
                def motif_limit(m: Array[Int]): Boolean = {
                        for (i <- 0 until size)
                                if (m(i) != i + 1)
                                        return false
                        return true
                }
                def motif_valid(m: Array[Int]): Boolean = {
                        var t:Array[Boolean] = fill(size){false}
                        for (i <- 0 until size)
                                t(m(i) - 1) = true
                        for (i <- 0 until size)
                                if (!t(i))
                                        return false
                        return true
                }
                def motif_incr(m: Array[Int]): Array[Int] = {
                        m(0) += 1
                        var i = 0
                        while (m(i) == size + 1)
                        {
                                m(i) = 1
                                i += 1
                                m(i) += 1
                        }
                        return m
                }
                def motif_value(m: Array[Int]): Int = {
                        var v = 0
                        for (i <- 0 until size)
                                v = v * (size + 1) + m(i)
                        return v
                }
		def value_motif(n: Int): Array[Int] = {
                        if (deb) println(n + " : ")
                        var p: Int = size - 1
                        var v: Int = n
                        var m: Array[Int] = Array.fill(size){1}
                        while (p >= 0)
                        {
                                m(p) = m(p) * (v % (size + 1))
                                p = p - 1
                                v = v / (size + 1)
                        }
                        return m
                }
                def motif_visibles(d: Int, f: Int, s: Int, m: Array[Int]): Int = {
                        var r: Int = 0
                        var l: Int = 0
                        for (i <- d to f by s)
                                if(m(i) > l) {
                                        r = r + 1
                                        l = m(i)
                        }
                        return r
                }
		def motif_gen(v: Int, g: Int, d: Int, i: Int): Unit = {
                        if (i == size)
                        {
                                var u = motif_value(motif)
                                if (deb) println((g + 1) + " " + (d + 1) + " " + motif(0) + motif(1) + motif(2) + motif(3) + " " + u)
                                if (motif_futur(g)(d)(u) == 0)
                                        motif_futur(g)(d)(u) = v
                                else if (motif_futur(g)(d)(u) > 0)
                                        motif_futur(g)(d)(u) = -1
                                return
                        }
                        var s = motif(i)
                        motif_gen(v, g, d, i + 1)
                        motif(i) = 0
                        motif_gen(v, g, d, i + 1)
                        motif(i) = s
                }
                while (!motif_limit(motif))
                {
                        motif = motif_incr(motif)
                        if (motif_valid(motif))
                        {
                                if (deb) println()
                                motif_gen(motif_value(motif),
                                        motif_visibles(0,size - 1, 1, motif) - 1,
                                        motif_visibles(size - 1, 0, -1, motif) - 1, 0)
                        }
                }
	}

	def game_starter (): Unit = {
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
		
		val difficulty:Int = string_game_parameters_def_list(0)._2 match{
                                case "Facile" => 0
                                case "Difficile" => 1
                                case "Libre" => 2
                                case _ => {println("Attention: Mode de difficulté non reconnu");2}
                }
		
		if (difficulty == 2)
		{
			Towers.init_map()
                        Towers.launch_game_timer()
                        Towers.game_frame_content.grid.get_contents.foreach(label => {
				label.init()
				label.clean()
			})
			return
		}

		var resoluble = false
		var debug_mode = string_game_parameters_def_list(2)._2==("Spectateur")
		Towers.init_futur(debug_mode)
		var th = new Thread {
			override def run {
				while (!resoluble)
				{
					Towers.game_frame_content.grid.get_contents.foreach(label => label.default())
					Towers.init_map()
					Towers.game_frame_content.grid.get_contents.foreach(label => label.init())
					resoluble = Towers.solver(debug_mode)
					if (difficulty == 1)
						resoluble = !resoluble
				}
				Towers.game_frame_content.grid.get_contents.foreach(label => label.clean())
				Towers.launch_game_timer()
			}
		}
		th.start()
	}
	def game_action_restart(): Unit = {
		Towers.game_frame_content.grid.get_contents.foreach(label => {
			label.init()
			label.clean()
		})
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


	def suppress_value(v: Int, cur_x: Int, cur_y: Int) = {
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

	def solver(deb: Boolean): Boolean = {
		var g = game_frame_content.grid
                var motif: Array[Int] = Array.fill(size){1}
		def motif_value(m: Array[Int]): Int = {
			var v = 0
			for (i <- 0 until size)
				v = v * (size + 1) + m(i)
			return v
		}
		def value_motif(n: Int): Array[Int] = {
			var p: Int = size - 1
			var v: Int = n
			var m: Array[Int] = Array.fill(size){1}
			while (p >= 0)
			{
				m(p) = m(p) * (v % (size + 1))
				p = p - 1
				v = v / (size + 1)
			}
			return m
		}
		if (deb)
		{
			println("--------------------------------------------\nSolveur activé\n")
			g.get_contents.foreach(label => {
				label.text = label.value.toString
	                	label.foreground = TGE.blue
			})
		}
		var nb_trouve = 0
		var continue = true
		while (continue && nb_trouve < size * size)
		{
			continue = false
			for (x <- 0 until size)
			{
				var m: Array[Int] = Array.fill(size){1}
				if (deb) println(border_labels(0)(x).condition + " " + border_labels(2)(x).condition)
				for (y <- 0 until size)
				{
					m(y) = g.access_xy(x, y).num
					if (deb) println(g.access_xy(x, y).num)
				}
				var gauche = border_labels(0)(x).condition - 1
				var droite = border_labels(2)(x).condition - 1
				var v = motif_futur(gauche)(droite)(motif_value(m))
				if (v > 0)
				{
					var m2 = value_motif(v)
					for (y <- 0 until size)
                                	{
						if (g.access_xy(x, y).num == 0)
						{
							nb_trouve = nb_trouve + 1
							continue = true
							if (deb) println("Nombre de cases dévoilées : " + nb_trouve)
						}
						g.access_xy(x, y).num = m2(y)
						if (deb)
                                                {
                                                        println(m2(y))
                                                        g.access_xy(x, y).foreground = TGE.red
                                                        g.access_xy(x, y).text = m2(y).toString
                                                }
                                	}
				}
				if (deb) Console.readLine
			}
			for (y <- 0 until size)
                        {
				var m: Array[Int] = Array.fill(size){1}
                                if (deb) println(border_labels(3)(y).condition + " " + border_labels(1)(y).condition)
                                for (x <- 0 until size)
                                {
                                        m(x) = g.access_xy(x, y).num
                                        if (deb) println(g.access_xy(x, y).num)
                                }
				var gauche = border_labels(3)(y).condition - 1
                                var droite = border_labels(1)(y).condition - 1
                                var v = motif_futur(gauche)(droite)(motif_value(m))
                                if (v > 0)
                                {
                                        var m2 = value_motif(v)
                                        for (x <- 0 until size)
                                        {
						if (g.access_xy(x, y).num == 0)
                                                {
							continue = true
                                                        nb_trouve = nb_trouve + 1
                                                        if (deb) println("Nombre de cases dévoilées : " + nb_trouve)
                                                }
						g.access_xy(x, y).num = m2(x)
                                                if (deb)
						{
							println(m2(x))
							g.access_xy(x, y).foreground = TGE.red
							g.access_xy(x, y).text = m2(x).toString
						}
                                        }
                                }
                                if (deb) Console.readLine
                        }
		}
		if (nb_trouve == size * size)
		{
			if (deb) println("Difficulté : Facile")
			return true
		}
		else
		{
			if (deb) println("Difficulté : Difficile")
			return false
		}
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

} //accolade fermante du package Towers

} //accolade fermante du package Games
