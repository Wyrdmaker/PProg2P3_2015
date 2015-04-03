import scala.swing._
import scala.swing.event._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat
import scala.math._
import javax.swing.{ImageIcon, Icon}

import Games.AngelWar._
import GUI._

package Games{
package AngelWar{

//"AWGE" -> "AngelWar_Graphical_Element"
object AWGE extends GUI_Graphical_Elements{
	val grey = new Color(96,96,96)
	val light_grey = new Color(160,160,160)

	def no_color_mode () = {
		//Le max est une sécurité. Si IndexOf ne trouve pas la chaine correspondant au mode de couleur dans la liste de ses valeurs possibles, il renvoie -1.
		//Ainsi, en cas de faute de frappe, le mode de couleur utilisé est le Normal
		max(0,AngelWar.string_game_parameters_def_list(0)._3.indexOf(AngelWar.string_game_parameters_def_list(0)._2))
	}

	def label_color_black () = {
		label_color_black_list(no_color_mode())
	}
	def label_color_white () ={
		label_color_white_list(no_color_mode)
	}

	val label_color_black_list = IndexedSeq(grey, violet_fluo)
	val label_color_white_list = IndexedSeq(white, cyan)

	//val bottom_panel_color_list = IndexedSeq(white, grey)

}

class AngelWar_Help_Frame extends Frame {
	title = "Aide"
	contents = new Label(){
		background = GUI_Mood.b_colour
		foreground = GUI_Mood.f_colour
		opaque = true
		text = "<html> <body> <head style=\"font-size:25px; font-family:arial;\"> <u> Aide de AngelWar </u> </head> <br> <p style=\"font-size:13px; font-family:arial;\">" +
		"Aidez les anges blancs à vaincre leurs homologues déchus en les disposant dans certaines des case de telles " +
		"sorte que les conditions suivantes soient remplies: <br>" +
		"	- Il y a autant d'anges blancs que de noirs <br> " +
		"	- Les anges doivent etre associés de telle sorte que chaque ange blanc est directement adjacent (diagonales exclues) à son " +
		"ange noir.<br>Cependant, l'ange blanc peut etre adjacent à d'autres anges noirs que le sien. <br>" +
		"	- Il n'y a pas d'anges blancs adjacents (diagonales comprises) à d'autres anges blancs <br>" +
		"	- Le nombre d'anges blancs de chaque ligne et chaque colonne correspond aux nombres indiqués sur les bords de la grille <br>" +
		" Pour vous aider, vous pouvez poser des marqueurs sur certaines cases, lorsque vous pensez qu'aucun ange ne devrait être sur cette case." + 
		"</p> </body> </html>"
	background = GUI_Mood.b_colour
	foreground = GUI_Mood.f_colour
	opaque = true
	}
	resizable = false
	visible = true
	centerOnScreen()
}

class AngelWar_About_Frame extends Frame{
	title = "A Propos"
	contents = new Label("Interface Graphique par T.Dupriez et G.Hocquet"){
		background = GUI_Mood.b_colour
		foreground = GUI_Mood.f_colour
		opaque = true
	}
	visible = true
}

object AngelWar extends Game{
	val title = "Angel War"

	val square_size_x = 50
	val square_size_y = 50
	var game_beginning_time: Date = null
	//var in_game = false héritée de Game

	//##Game parameters##
	var numeric_game_parameters_def_list = IndexedSeq(("Largeur", 0, 8, 20), ("Hauteur", 0, 8, 20))
	var string_game_parameters_def_list = IndexedSeq(("Ambiance", "Assaut Céleste", IndexedSeq("Assaut Céleste", "Revanche Infernale")))
	def nb_of_rows = numeric_game_parameters_def_list(1)._2  //fait de nb_of_rows un alias de la valeur du paramètre Height (ne marche que pour la lecture)
	def nb_of_cols = numeric_game_parameters_def_list(0)._2  //fait de nb_of_cols un alias de la valeur du paramètre Width (ne marche que pour la lecture)
	def color_parameter = string_game_parameters_def_list(0)._2
	val has_numeric_parameters_0asWidth_1asHeight = true

	type Game_Label_Class = AngelWar_Label
	def glb_factory () = { new Game_Label_Class } // "glb" -> "Game_Label_Class"
	type Game_Border_Label_Class = AngelWar_Label
	def gblb_factory () = {new Game_Border_Label_Class}
	def about_frame_factory () = { new AngelWar_About_Frame }
	def help_frame_factory () = { new AngelWar_Help_Frame }

	//var random_gen héritée de Game
	//var game_frame_content héritée de Game

	main_character_text_on_launching = main_character_text_on_launching ++ Array("Ces anges...<br>Toujours en guerre.<br>Moi, ça ferait longtemps que j'aurais gagné","Si tu en as marre de jouer les gentils, essaie le mode \"Revanche Infernale\".")
	main_character_text_on_win = Array("Un vrai stratège !")

	val game_game_mode_list = IndexedSeq(
		Game_Mode(IndexedSeq(8,8),IndexedSeq("Assaut Céleste")),
		Game_Mode(IndexedSeq(10,10),IndexedSeq("Assaut Céleste")),
		Game_Mode(IndexedSeq(15,15),IndexedSeq("Assaut Céleste"))
	)
	def custom_game_parameters_conditions (form_nb_fields_result: IndexedSeq[Int]) ={
		var return_value = "OK"
		return_value
				
	}

	def mod_board(board: Array[Array[Array[Int]]],x:Int,y:Int,tipe:Int = -5,free:Int = -5,x_ass:Int = -5,y_ass:Int = -5) = {
		if(tipe != -5){board(x)(y)(0) = tipe}
		if(free != -5){board(x)(y)(1) = free}
		if(x_ass != -5){board(x)(y)(2) = x_ass}
		if(y_ass != -5){board(x)(y)(3) = y_ass}
	}

	def apply_f_to_adjacent_squares(xi:Int,yi:Int,include_diag:Boolean,f:((Int,Int) => Unit)) ={
	//Applique f(x,y) pour tout les (x,y) des cases adjacentes à(xi,yi) (diagonales incluses si include_diag = true)
		if (yi > 0) {
			if(include_diag){if(xi > 0) {f(xi-1, yi-1)}}			//top left
			f(xi, yi-1)												//top
			if(include_diag){if(xi < nb_of_cols-1){f(xi+1, yi-1)}}	//top right
		}
		if(xi > 0){ f(xi-1, yi)}									//left
		if(xi < nb_of_cols-1){f(xi+1, yi)}							//right
		if(yi < nb_of_rows-1){
			if(include_diag){if(xi > 0){f(xi-1, yi+1)}}				//bottom left
			f(xi, yi+1)												//bottom
			if(include_diag){if(xi < nb_of_cols-1){f(xi+1, yi+1)}}	//bottom right
		}
	}

	def unfree_adjacent_squares(board: Array[Array[Array[Int]]],x:Int, y:Int) = {
		mod_board(board,x,y,free=0)
		apply_f_to_adjacent_squares(x,y,true,(x,y)=>mod_board(board,x,y,free=0))
	}

	def game_starter () ={
		def f_available_spaces_for_tent (x:Int,y:Int) : Array[(Int,Int)] = {
			//renvoie l'ensemble des case adjacentes(diagonales exclues) pouvant accueillir une tente (cases vides et non-adjacentes(diagonales incluses) à d'autres tentes)	
			var result: Array[(Int,Int)] = Array()
			def f_to_apply(x:Int,y:Int)={
				if(solved_board(x)(y)(1) == 1){
					result = result :+ (x,y)
				}
			}
			apply_f_to_adjacent_squares(x,y,false,f_to_apply)
			result
		}

		var square_array = Array.range(0, nb_of_cols*nb_of_rows)
		var square_route : Array[(Int, Int)]= Array()

		def regen () ={
			//Cette fonction sera réappelée dans la boucle plus loin lorsque le processus de remplissage de la grille sera dans un cul-de-sac
			solved_board = Array.fill(nb_of_cols, nb_of_rows)(Array(0, 1, -1, -1))
			game_board = Array.fill(nb_of_cols, nb_of_rows)(Array(0, 1, -1, -1))
			//Représente le plateau d'une partie: cf lieu de la définition de ces variables, plus bas
			//Convention: La matrice est un tableau ligne (x) de tableaux colonnes (y)
			//Convention: Les cases sont comptées de gauche à droite et de haut en bas
			

			//Choisir un ordre de parcours des cases pour y poser éventuellement des arbres
			square_array = Array.range(0, nb_of_cols*nb_of_rows)
			square_route = Array()
			def n_to_xy (n: Int) : (Int, Int) ={
				//convertit un numéro de case en le couple des coordonnées de la case
				(n % nb_of_cols, n / nb_of_cols)
			}
			for (i <- 0 until nb_of_cols*nb_of_rows){
				var random_n = random_gen.nextInt(square_array.length)
				square_route = square_route :+ n_to_xy(square_array(random_n))
				for (j <- random_n until square_array.length){
					square_array(j) = square_array(j) + 1
				}
				square_array = square_array.dropRight(1)
			}
		}
		regen()
		var nb_of_trees = (ceil((nb_of_cols * nb_of_rows) / 5) - 1).toInt
		var square_route_index = 0
		
		while (nb_of_trees > 0) {
			if(square_route_index < nb_of_rows*nb_of_cols){
				val random_square = square_route(square_route_index)
				val x = random_square._1
				val y = random_square._2
				if (solved_board(x)(y)(0) == 0){	//case vide
					val available_spaces_for_tent = f_available_spaces_for_tent(x, y)
					if (available_spaces_for_tent.length != 0) {
						var random_place = (0,0)
						if (available_spaces_for_tent.length >1){random_place = available_spaces_for_tent(random_gen.nextInt(available_spaces_for_tent.length - 1))}
						else {random_place = available_spaces_for_tent(0)}
						mod_board(solved_board,x,y,tipe=1,free=0,x_ass = random_place._1, y_ass= random_place._2)//ajouter l'arbre			
						mod_board(solved_board,random_place._1, random_place._2, tipe=2, free =0, x_ass=x, y_ass=y)	//ajouter la tente
						unfree_adjacent_squares(solved_board,random_place._1, random_place._2)
						nb_of_trees = nb_of_trees - 1
					}

				}
				square_route_index = square_route_index + 1
			}
			else{
				regen()
				square_route_index = 0
				nb_of_trees = (ceil((nb_of_cols * nb_of_rows) / 5) - 1).toInt
			}
		}
		game_board = solved_board

		//Construire les tableaux rows_conditions et cols_conditions en comptant les tentes
		rows_conditions = Array()
		for (y<- 0 until nb_of_rows){
			var nb_of_tents = 0
			for (x<- 0 until nb_of_cols){
				if (game_board(x)(y)(0) == 2){
					nb_of_tents = nb_of_tents + 1
				}
			}
			rows_conditions = rows_conditions :+ nb_of_tents
		}
		cols_conditions = Array()
		for (x<- 0 until nb_of_cols){
			var nb_of_tents = 0
			for (y<- 0 until nb_of_rows){
				if (game_board(x)(y)(0) == 2){
					nb_of_tents = nb_of_tents + 1
				}
			}
			cols_conditions = cols_conditions :+ nb_of_tents
		}

		game_frame_content.set_right_border_grid()
		right_border_labels = game_frame_content.right_border_grid.get_contents()
		game_frame_content.set_bottom_border_grid()
		bottom_border_labels =game_frame_content.bottom_border_grid.get_contents()
		game_frame_content.set_bottom_right_border_label()
		game_frame_content.bottom_right_border_label.background = AWGE.firebrick4
		game_frame_content.bottom_right_border_label.opaque = true

		//Définir l'image de background du gridpanel
		val hell_background_icon = new ImageIcon(getClass.getResource("/AngelWar/pics-of-hell.png"))
		val hell_background_image = hell_background_icon.getImage()
		val heaven_background_icon = new ImageIcon(getClass.getResource("/AngelWar/Nature-Clouds-Heaven-.jpg"))
		val heaven_background_image = heaven_background_icon.getImage()
		var img = hell_background_image
		AWGE.no_color_mode() match {
			case 1 => img = heaven_background_image
			case 0 => img = hell_background_image
			case _ => img = hell_background_image
		}
		game_frame_content.grid.set_image_background(img, 2, 2, 2, 2)

		//nettoyer game_board et initial_game_board des élèments de solution et initialiser les labels (autres que les labels de condition) de la grille
		for (x<- 0 until nb_of_cols){
			for (y <- 0 until nb_of_rows){
				game_board(x)(y)(0) match {
					case 1 => {
						mod_board(game_board,x, y, free=0, x_ass= -1, y_ass= -1)	//arbre
					}
					case _ => {
						mod_board(game_board,x, y, tipe = 0, free = 1, x_ass = -1, y_ass = -1)	//tente ou vide
					}
				}
				game_frame_content.grid.access_xy(x, y).init(game_board(x)(y)(0))
			}	
		}

		//initialiser les labels de conditions de la grille
		for (y <- 0 until nb_of_rows){
			right_border_labels(y).init(3, rows_conditions(y))
		}
		for (x <- 0 until nb_of_cols){
			bottom_border_labels(x).init(3, cols_conditions(x))
		}

		initial_game_board = game_board.clone()

		AngelWar.launch_game_timer()
	}
	def game_action_restart() : Unit = {
		game_board = initial_game_board.clone()
		//Initialise chaque label selon le board
		for (x<- 0 until nb_of_cols){
			check_rows_condition(0,x)//c'est bien x mais vu comme un y
			check_cols_condition(x,0)
			for (y <- 0 until nb_of_rows){
				game_frame_content.grid.access_xy(x, y).init(game_board(x)(y)(0))
			}	
		}
		AngelWar.launch_game_timer()
	}
	//Définit ce qui se passe en cas de victoire du joueur -> voir Game
	override def win() = {
		super.win()
	}
	//Définit ce qui se passe en cas de défaite du joueur -> voir Game
	override def lose() = {
		super.lose()
	}

	//##AngelWar Variables## // Variables internes au AngelWar
	//solved_board et game_board sont des matrices de tableaux de quatres entiers:
	//(type_de_la_case, case_libre_pour_une_tente?, x_de_la_case_associée, y_de_la_case_associée)	
	//type_de_la_case: 0 pour "empty" | 1 pour "tent" | 2 pour "tree" | 3 pour les labels de condition 
	//case_libre_pour_une_tente vaut 1 s'il n'y a aucune tente sur une case voisine (diagonales incluses) et 0 sinon
	//case_associée: arbre associé à une tente ou tente associée à un arbre (-1 signifie pas de case associée)
	var solved_board: Array[Array[Array[Int]]] = Array()	//Plateau résolu, généré en début de partie
	var initial_game_board: Array[Array[Array[Int]]] = Array()	//Sauvegarde de game_board, juste avant le début de la partie	
	var game_board: Array[Array[Array[Int]]] = Array()	//Plateau de jeu, utilisé pour le jeu
	var rows_conditions: Array[Int] = Array()	//Liste des nb de tentes pour chaque ligne
	var cols_conditions: Array[Int] = Array()	//Liste des nb de tentes pour chaque colonnes
	var error_nb: Int = 0 //Nombre d'erreurs, empèche de gagner, géré par les labels
	var right_border_labels :	Seq[Game_Border_Label_Class] = null
	var bottom_border_labels: Seq[Game_Border_Label_Class] = null

	//##AngelWar Functions## //Fonctions internes au AngelWar
	def check_error(x:Int,y:Int) ={
		//vérification du nb de tentes adjacentes
		var adjacent_tent_nb = 0
		def f_to_apply(x:Int,y:Int) = {
			if(game_board(x)(y)(0)==2){
				adjacent_tent_nb += 1
			}
		}

		apply_f_to_adjacent_squares(x,y,true,f_to_apply)
		if (adjacent_tent_nb>0){game_frame_content.grid.access_xy(x,y).set_adj_tent_error}
		else{game_frame_content.grid.access_xy(x,y).unset_adj_tent_error}

		//vérification de la rows condition sur la rangée de la case (x,y)
		check_rows_condition(x,y)
		//vérification de la cols condition sur la colonne de la case (x,y)
		check_cols_condition(x,y)
	}

	def assoc(x1:Int, y1:Int, x2:Int, y2:Int)={
		//modifie x_ass et y_ass de game_board pour que les deux cases spécifiées soient associées
		mod_board(game_board,x1, y1, x_ass=x2, y_ass=y2)
		mod_board(game_board,x2, y2, x_ass=x1, y_ass=y1)
	}

	def unassoc(x:Int, y:Int)={
		//modifie x_ass et y_ass de game_board pour que la case (x,y) et sa case associée ne le soient plus
		val x2 = game_board(x)(y)(2)
		val y2 = game_board(x)(y)(3)
		mod_board(game_board,x, y, x_ass= -1, y_ass= -1)
		if (x2 >=0 && y2 >= 0){
			mod_board(game_board,x2, y2, x_ass= -1, y_ass= -1)
		}
	}

	def find_tree(x:Int, y:Int, reserved_tree_xy_list: Array[(Int,Int)] = Array()) :Boolean={
		//Cherche un arbre adjacent non-associé pour l'associer à la case (x,y)
		//Si un tel arbre existe, associe la case (x,y) à cet arbre et renvoie true
		//Si il n'y a pas d'arbre adjacent, indique au label de se mettre en mode erreur
		//Si il y a des arbres adjacents mais qu'ils sont déjà associé, cf plus loin
		//le paramètre reserved_tree_xy_list liste les coordonnée des arbres volés, que les fonctions appelées ne peuvent donc pas revoler
		val this_square = game_board(x)(y)
		if(this_square(0) != 2){return(false)}	//ce n'est pas une tente
		if(this_square(2) != -1 && this_square(3) != -1){
			val assoc_square = game_board(this_square(2))(this_square(3))
			if(assoc_square(2) == x && assoc_square(3) == y && reserved_tree_xy_list.indexOf((assoc_square(2),assoc_square(3))) == -1){
				game_frame_content.grid.access_xy(x,y).unset_no_adj_tree_error
				return(true) //cette tente est déja associée à un arbre n'étant pas dans la liste des arbres interdits
			}
		}
		var assoc_tree_xy_list : Array[(Int, Int)] =Array()
		var return_true = false
		def f_to_apply (x2:Int,y2:Int)={
			if(!return_true){
				val pot_tree = game_board(x2)(y2)	//"pot_tree" -> "Arbre potentiel"
				if (pot_tree(0)==1){	//pot_tree est bien un arbre
					if (pot_tree(2) == -1 && pot_tree(3) == -1){	//arbre libre trouvé
						assoc(x,y,x2,y2)
						game_frame_content.grid.access_xy(x,y).unset_no_adj_tree_error
						return_true = true
					}
					else {assoc_tree_xy_list = assoc_tree_xy_list :+ (x2,y2)}	//arbre associé trouvé
				}
			}			
		}
		apply_f_to_adjacent_squares(x,y,false,f_to_apply)
		if(return_true){return(true)}
		//aucun arbre libre trouvé, on regarde si on a trouvé des arbres associés
		if (assoc_tree_xy_list.length == 0) {	//La tente placée ne peut correspondre à aucun arbre
			game_frame_content.grid.access_xy(x,y).set_no_adj_tree_error
			return(false)
		}
		else{
			var other_tent_x = 0
			var other_tent_y = 0
			for (i <- 0 until assoc_tree_xy_list.length) {//on vole l'arbre en question et on voit si l'autre tente est capable de se trouver un autre arbre
				def predicate(xy:(Int,Int)) = {xy == assoc_tree_xy_list(i)}
				if (!(reserved_tree_xy_list.exists(predicate))){//on vérifie que l'arbre n'a pas déjà été volé
					var assoc_tree_xy = assoc_tree_xy_list(i)
					other_tent_x = game_board(assoc_tree_xy._1)(assoc_tree_xy._2)(2)
					other_tent_y = game_board(assoc_tree_xy._1)(assoc_tree_xy._2)(3)				
					unassoc(assoc_tree_xy._1, assoc_tree_xy._2)
					assoc(x, y, assoc_tree_xy._1, assoc_tree_xy._2)
					if(find_tree(other_tent_x, other_tent_y, reserved_tree_xy_list :+ assoc_tree_xy_list(i))){//l'autre tente a réussi à se trouver un autre arbre, on garde celui qu'on avait volé
						game_frame_content.grid.access_xy(x,y).unset_no_adj_tree_error
						return(true)
					}
					else{	//l'autre tente n'a pas réussi à se trouver un autre arbre, on restore les associations et on continue avec l'arbre associé suivant
						unassoc(x,y)
						assoc(other_tent_x, other_tent_y, assoc_tree_xy._1, assoc_tree_xy._2)
					}
				}	
			}
			//cette tente ne peut s'associer à aucun arbre
			game_frame_content.grid.access_xy(x,y).set_no_adj_tree_error()
			return(false)
		}
	}

	def adj_find_tree(x:Int, y:Int, not_this_one:Boolean) = {
		//Les tentes adjacentes à cet arbre doivent se trouver un autre arbre, si elles échouent, set_error_no_adj_tree
		//Si not_this_one est vrai les tentes adjacentes doivent chercher un autre arbre que celui de la case (x,y)
		var reserved_list: Array[(Int,Int)] = Array()
		if(not_this_one){
			reserved_list = Array((x,y))
		}
		if(game_board(x)(y)(0) == 1){
			def f_to_apply(x:Int,y:Int) ={
				if(!find_tree(x,y,reserved_list) && game_board(x)(y)(0) == 2){
					game_frame_content.grid.access_xy(x,y).set_no_adj_tree_error()
				}
				else{game_frame_content.grid.access_xy(x,y).unset_no_adj_tree_error()}			
			}
			apply_f_to_adjacent_squares(x,y,false,f_to_apply)
		}
	}
		
	def check_rows_condition(x:Int, y:Int) =  {
		//vérification de la rows condition sur la rangée de la case (x,y)
		var nb_of_tents = 0
		for (cx<- 0 until nb_of_cols){
			if (game_board(cx)(y)(0) == 2){
				nb_of_tents = nb_of_tents + 1
			}
		}
		if(nb_of_tents > rows_conditions(y)){right_border_labels(y).set_condition_error}
		else {right_border_labels(y).unset_condition_error}
	}

	def check_cols_condition(x:Int, y:Int) = {
		//vérification de la cols condition sur la colonne de la case (x,y)
		var nb_of_tents = 0
		for (cy<- 0 until nb_of_rows){
			if (game_board(x)(cy)(0) == 2){
				nb_of_tents = nb_of_tents + 1
			}
		}
		if(nb_of_tents > cols_conditions(x)){bottom_border_labels(x).set_condition_error}
		else {bottom_border_labels(x).unset_condition_error}		
	}

	def check_adj_tent (x:Int, y:Int) :(Boolean, Array[(Int, Int)])= {
		//renvoie vrai s'il y à une tente adjacente, faux sinon et renvoie également la liste des coordonnées des tentes adjacentes
		var adjacent_tents: Array[(Int, Int)] = Array()
		def f_to_apply (x:Int,y:Int)={
			if(game_board(x)(y)(0)==2){
				adjacent_tents = adjacent_tents :+ (x, y)
			}		
		}
		apply_f_to_adjacent_squares(x,y,true,f_to_apply)
		if (adjacent_tents.length>0){return((true, adjacent_tents))}
		else{return((false, adjacent_tents))}		
	}

	def add_tent(x:Int, y:Int) :Unit= {	//appelée par un label se changeant en tente à la case (x, y)
		mod_board(game_board,x, y, tipe = 2, free=0)
		unfree_adjacent_squares(game_board,x,y)
		check_rows_condition(x,y)
		check_cols_condition(x,y)
		var adjacent_tent = false 	//Pour savoir s'il y  au moins une tente adjacente
		def f_to_apply(x:Int,y:Int)={
			if(game_board(x)(y)(0) == 2){
				game_frame_content.grid.access_xy(x,y).set_adj_tent_error()
				adjacent_tent = true
			}
		}
		apply_f_to_adjacent_squares(x,y,true,f_to_apply)	//set_adj_tent_error pour les tentes adjacentes
		if(adjacent_tent){game_frame_content.grid.access_xy(x,y).set_adj_tent_error()}	//set_adj_tent_error pour cette case s'il y a au moins une tente adjacente
		val adj_tents = check_adj_tent(x,y)
		if(! find_tree(x,y)){
			if(adj_tents._1){
				adj_tents._2.foreach(xy => game_frame_content.grid.access_xy(xy._1,xy._2).set_adj_tent_error())
				game_frame_content.grid.access_xy(x,y).set_adj_tent_error()
			}
			def f2_to_apply(x:Int,y:Int) ={
				adj_find_tree(x,y,true)	
			}
			apply_f_to_adjacent_squares(x,y,false,f2_to_apply)
		}

		check_win()
	}

	def restore_square_freedom(x:Int, y:Int) ={
		val adjacent_tents = check_adj_tent(x,y)
		if(!adjacent_tents._1){	//pas de tentes adjacentes
			mod_board(game_board,x,y,free=1)
			game_frame_content.grid.access_xy(x,y).unset_adj_tent_error()
		}
	}

	def restore_adjacent_square_freedom(x:Int, y:Int) ={
		def f_to_apply (x:Int,y:Int)={
			restore_square_freedom(x,y)	
		}
		apply_f_to_adjacent_squares(x,y,true,f_to_apply)
	}

	def clear_no_assoc_tree_error()={	//retire une éventuelle no_adj_tree_error d'un label qui n'est pas dans ce cas
		for(x <- 0 until nb_of_cols){
			for (y <- 0 until nb_of_rows){
				val square = game_board(x)(y)
				if (square(0) == 2){	//la case est une tente
					if (square(2) == -1 || square(3) == -1){game_frame_content.grid.access_xy(x,y).set_no_adj_tree_error()}
					else{
						val assoc_tree = game_board(square(2))(square(3))
						if (x != assoc_tree(2) || y != assoc_tree(3)){	//l'arbre n'est pas réciproquement associé à la tente
							game_frame_content.grid.access_xy(x,y).set_no_adj_tree_error()
							mod_board(game_board,x,y,x_ass= -1,y_ass = -1 )
						}
						else{	//La tente est associé à un arbre et l'arbre est associé à la tente
							game_frame_content.grid.access_xy(x,y).unset_no_adj_tree_error()
						}
					}
				}
			}
		}
	}

	def remove_tent(x:Int, y:Int) = {	//est appelée par un label se débarassant de sa tente à la case (x,y)
		val x_ass = game_board(x)(y)(2)
		val y_ass = game_board(x)(y)(3)
		unassoc(x,y)
		mod_board(game_board,x, y, tipe = 0, free=1)
		restore_adjacent_square_freedom(x,y)
		check_error(x,y)
		if(x_ass != -1 && y_ass != -1){adj_find_tree(x_ass,y_ass,false)}
		/*def f_to_apply(x:Int,y:Int) ={
			adj_find_tree(x,y,false)
		}
		apply_f_to_adjacent_squares(x,y,false,f_to_apply)*/
		//clear_no_assoc_tree_error()
		check_win()
	}

def check_win() ={
		//vérifie que tout les arbres sont associés
		var unassociated_tree = false
		for(x <- 0 until nb_of_cols){
			for(y <- 0 until nb_of_rows){
				if (game_board(x)(y)(0) == 1){
					val tree = game_board(x)(y)
					if(tree(2)== -1 || tree(3)== -1){unassociated_tree = true}
					else{if(game_board(tree(2))(tree(3))(0) != 2){ unassociated_tree=true}}
				}
			}
		}
		if (!unassociated_tree){
			if (error_nb == 0){win()}	
		}
	}
}

} //accolade fermante du package AngelWar

} //accolade fermante du package Games
