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
	contents = new Label("Retournez les cases en leur cliquant dessus jusqu'à ce qu'elles soient toutes blanches"){
		background = GUI_Mood.b_colour
		foreground = GUI_Mood.f_colour
		opaque = true
	}
	visible = true
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
	var numeric_game_parameters_def_list = IndexedSeq(("Largeur", 0, 8, 25), ("Hauteur", 0, 8, 25))
	var string_game_parameters_def_list = IndexedSeq(("Mode de Couleur", "Classique", IndexedSeq("Classique")))
	def nb_of_rows = numeric_game_parameters_def_list(1)._2  //fait de nb_of_rows un alias de la valeur du paramètre Height (ne marche que pour la lecture)
	def nb_of_cols = numeric_game_parameters_def_list(0)._2  //fait de nb_of_cols un alias de la valeur du paramètre Width (ne marche que pour la lecture)
	def color_parameter = string_game_parameters_def_list(0)._2
		
	//Conservé pour futurs références mais inutile dans le démineur
	/*def nb_of_bombs = game_parameter_1 //Ces deux fonctions font de nb_of_bombs un alias de la variable game_parameter_1
	def nb_of_bombs_=(newval: Int) { game_parameter_1 = newval }*/

	type Game_Label_Class = AngelWar_Label
	def glb_factory () = { new Game_Label_Class } // "glb" -> "Game_Label_Class"
	def about_frame_factory () = { new AngelWar_About_Frame }
	def help_frame_factory () = { new AngelWar_Help_Frame }

	//var random_gen héritée de Game
	//var game_frame_content héritée de Game

	val game_game_mode_list = IndexedSeq(
		Game_Mode(IndexedSeq(8,8),IndexedSeq("Classique")),
		Game_Mode(IndexedSeq(10,10),IndexedSeq("Classique")),
		Game_Mode(IndexedSeq(15,15),IndexedSeq("Classique"))
	)
	def custom_game_parameters_conditions (form_nb_fields_result: IndexedSeq[Int]) ={
		var return_value = "OK"
		return_value
				
	}	

	def unfree_adjacent_squares(x:Int, y:Int) = {
		//met à 0 le caractère free des cases adjacentes (diagonales incluses)
		if (y > 0) {
			if(x > 0) {mod_solved_board(x-1, y-1, free=0)}				//top left
			mod_solved_board(x, y-1, free=0)							//top
			if(x < nb_of_cols-1){mod_solved_board(x+1, y-1, free=0)}	//top right
		}
		if(x > 0){ mod_solved_board(x-1, y, free=0)}					//left
		if(x < nb_of_cols-1){mod_solved_board(x+1, y, free=0)}			//right
		if(y < nb_of_rows-1){
			if(x > 0){mod_solved_board(x-1, y+1, free=0)}				//bottom left
			mod_solved_board(x, y+1, free=0)							//bottom
			if(x < nb_of_cols-1){mod_solved_board(x+1, y+1, free=0)}	//bottom right
		}
		mod_solved_board(x, y, free=0)
	}

	def mod_solved_board(x:Int, y:Int, tipe:Int = -5, free:Int = -5, x_ass:Int = -5, y_ass:Int = -5) ={
		//Pour modifier des éléments de solved_board
		var tipeb = 0
		var freeb = 0
		var x_assb = 0
		var y_assb = 0
		if(tipe == -5){tipeb = solved_board(x)(y)(0)} else {tipeb = tipe}
		if(free == -5){freeb = solved_board(x)(y)(1)} else {freeb = free}
		if(x_ass == -5){x_assb = solved_board(x)(y)(2)} else {x_assb = x_ass}
		if(y_ass == -5){y_assb = solved_board(x)(y)(3)} else {y_assb = y_ass}
		solved_board.update(x, solved_board(x).updated(y, Array(tipeb, freeb, x_assb, y_assb)))
	}

	def mod_game_board(x:Int, y:Int, tipe:Int = -5, free:Int = -5, x_ass:Int = -5, y_ass:Int = -5) ={
		//Pour modifier des éléments de solved_board
		var tipeb = 0
		var freeb = 0
		var x_assb = 0
		var y_assb = 0
		if(tipe == -5){tipeb = game_board(x)(y)(0)} else {tipeb = tipe}
		if(free == -5){freeb = game_board(x)(y)(1)} else {freeb = free}
		if(x_ass == -5){x_assb = game_board(x)(y)(2)} else {x_assb = x_ass}
		if(y_ass == -5){y_assb = game_board(x)(y)(3)} else {y_assb = y_ass}
		game_board.update(x, game_board(x).updated(y, Array(tipeb, freeb, x_assb, y_assb)))
	}

	def game_starter () = {
		/*for (i <- 0 until nb_of_cols) {
			for ( j <- 0 until nb_of_rows) {
				for( k <- 0 to 3) {
					print(" " + solved_board(i)(j)(k))	
				}
				print(" / ")

			}
			println()
		}*/
		def f_available_spaces_for_tent (x:Int, y:Int) : Array[(Int, Int)] = {
			//renvoie l'ensemble des case adjacentes(diagonales exclues) pouvant accueillir une tente (cases vides et non-adjacentes(diagonales incluses) à d'autres tentes)
			var result: Array[(Int, Int)] = Array()
			if (y != 0) {//la case n'est pas tout en haut
				if (solved_board(x)(y-1)(1) == 1) {
					result = result :+ (x, y-1)
				}
			}
			if (x != 0) {//la case n'est pas tout à gauche
				if (solved_board(x-1)(y)(1) == 1) {
					result = result :+ (x-1, y)
				}
			}
			if (x != nb_of_cols - 1) {//la case n'est pas tout à droite
				if (solved_board(x+1)(y)(1) == 1) {
					result = result :+ (x+1, y)
				}
			}
			if (y != nb_of_rows -1 ) {//la case n'est pas tout en bas
				if (solved_board(x)(y+1)(1) == 1) {
					result = result :+ (x, y+1)
				}
			}
			result
		}

		var square_array = Array.range(0, nb_of_cols*nb_of_rows)
		var square_route : Array[(Int, Int)]= Array()

		def regen () ={
			//Cette fonction sera réappelée dan la boucle plus loin lorsque le processus de remplissage de la grille sera dans un cul-de-sac
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
					//println(x); println(y)
					val available_spaces_for_tent = f_available_spaces_for_tent(x, y)
					if (available_spaces_for_tent.length != 0) {
						var random_place = (0,0)
						if (available_spaces_for_tent.length >1){random_place = available_spaces_for_tent(random_gen.nextInt(available_spaces_for_tent.length - 1))}
						else {random_place = available_spaces_for_tent(0)}
						mod_solved_board(x, y, tipe=1, free=0, x_ass = random_place._1, y_ass= random_place._2)	//ajouter l'arbre			
						mod_solved_board(random_place._1, random_place._2, tipe=2, free =0, x_ass=x, y_ass=y)	//ajouter la tente
						unfree_adjacent_squares(random_place._1, random_place._2)
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
		initial_game_board = game_board

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
		
		//Construire une nouvelle grille avec une rangée et une colonne en plus pour les labels affichant les rows/cols conditions
		Game_Parameters_Value_Setters.numeric_game_parameter_value_setter(0, nb_of_cols + 1, AngelWar)
		Game_Parameters_Value_Setters.numeric_game_parameter_value_setter(1, nb_of_rows + 1, AngelWar)
		game_frame_content = new Game_Frame_Content[AngelWar_Label] (AngelWar)
		Game_Parameters_Value_Setters.numeric_game_parameter_value_setter(0, nb_of_cols - 1, AngelWar)
		Game_Parameters_Value_Setters.numeric_game_parameter_value_setter(1, nb_of_rows - 1, AngelWar)
		UI_Link.actual_ui.contents = game_frame_content


		//nettoyer game_board des élèments de solution et initialiser les labels (autres que les labels de condition) de la grille
		for (x<- 0 until nb_of_cols){
			for (y <- 0 until nb_of_rows){
				game_board(x)(y)(0) match {
					case 1 => mod_game_board(x, y, free=0, x_ass= -1, y_ass= -1)	//arbre
					case _ => mod_game_board(x, y, tipe = 0, free = 1, x_ass = -1, y_ass = -1)	//tente
				}
				game_frame_content.grid.access_xy(x, y).init(game_board(x)(y)(0))
			}	
		}
		
		//initialiser les labels de conditions de la grille
		for (y <- 0 until nb_of_rows){
			game_frame_content.grid.access_xy(nb_of_cols, y).init(3, rows_conditions(y))
		}
		for (x <- 0 until nb_of_cols){
			game_frame_content.grid.access_xy(x, nb_of_rows).init(3, cols_conditions(x))
		}
		game_frame_content.grid.access_xy(nb_of_cols, nb_of_rows).init(3, -1)
	
		AngelWar.launch_game_timer()

	}
	def game_action_restart() : Unit = {
		AngelWar.game_board = AngelWar.initial_game_board
		//Initialise chaque label selon le board
		for (x<- 0 until nb_of_cols){
			for (y <- 0 until nb_of_rows){
				game_frame_content.grid.access_xy(x, y).init(game_board(x)(y)(0))
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
	//##AngelWar Functions## //Fonctions internes au AngelWar

	def check_error(x:Int, y:Int) ={
		//vérification du nb de tentes adjacentes
		var adjacent_tent_nb = 0
		if (y > 0) {
			if(x > 0) {if(solved_board(x-1)(y-1)(0)==2){adjacent_tent_nb = adjacent_tent_nb+1}}				//top left
			if(solved_board(x)(y-1)(0)==2){adjacent_tent_nb = adjacent_tent_nb+1}							//top
			if(x < nb_of_cols-1){if(solved_board(x+1)(y-1)(0)==2){adjacent_tent_nb = adjacent_tent_nb+1}}	//top right
		}
		if(x > 0){if(solved_board(x-1)(y)(0)==2){adjacent_tent_nb = adjacent_tent_nb+1}}					//left
		if(x < nb_of_cols-1){if(solved_board(x+1)(y)(0)==2){adjacent_tent_nb = adjacent_tent_nb+1}}			//right
		if(y < nb_of_rows-1){
			if(x > 0){if(solved_board(x-1)(y+1)(0)==2){adjacent_tent_nb = adjacent_tent_nb+1}}				//bottom left
			if(solved_board(x)(y+1)(0)==2){adjacent_tent_nb = adjacent_tent_nb+1}							//bottom
			if(x < nb_of_cols-1){if(solved_board(x+1)(y+1)(0)==2){adjacent_tent_nb = adjacent_tent_nb+1}}	//bottom right
		}
		if (adjacent_tent_nb>0){game_frame_content.grid.access_xy(x,y).set_adj_tent_error}
		else{game_frame_content.grid.access_xy(x,y).unset_adj_tent_error}

		//vérification de la rows condition sur la rangée de la case (x,y)
		var nb_of_tents = 0
		for (cx<- 0 until nb_of_cols){
			if (game_board(cx)(y)(0) == 2){
				nb_of_tents = nb_of_tents + 1
			}
		}
		if(nb_of_tents > rows_conditions(y)){game_frame_content.grid.access_xy(nb_of_cols,y).set_condition_error}
		else {game_frame_content.grid.access_xy(nb_of_cols,y).unset_condition_error}

		//vérification de la cols condition sur la colonne de la case (x,y)
		nb_of_tents = 0
		for (cy<- 0 until nb_of_rows){
			if (game_board(x)(cy)(0) == 2){
				nb_of_tents = nb_of_tents + 1
			}
		}
		if(nb_of_tents > cols_conditions(x)){game_frame_content.grid.access_xy(x,nb_of_rows).set_condition_error}
		else {game_frame_content.grid.access_xy(x,nb_of_rows).unset_condition_error}
	}

	def assoc(x1:Int, y1:Int, x2:Int, y2:Int)={
		//modifie x_ass et y_ass de game_board pour que les deux cases spécifiées soient associées
		mod_game_board(x1, y1, x_ass=x2, y_ass=y2)
		mod_game_board(x2, y2, x_ass=x1, y_ass=y1)
	}

	def unassoc(x:Int, y:Int)={
		//modifie x_ass et y_ass de game_board pour que la case (x,y) et sa case associée ne le soient plus
		val x2 = game_board(x)(y)(3)
		val y2 = game_board(x)(y)(3)
		mod_game_board(x, y, x_ass= -1, y_ass= -1)
		if (x2 >=0 && y2 >= 0){
			mod_game_board(x2, y2, x_ass= -1, y_ass= -1)
		}
	}

	def find_tree(x:Int, y:Int, reserved_tree_xy_list: Array[(Int,Int)] = Array()) :Boolean={
		//Cherche un arbre adjacent non-associé pour l'associer à la case (x,y)
		//Si un tel arbre existe, associe la case (x,y) à cet arbre et renvoie true
		//Si il n'y a pas d'arbre adjacent, indique au label de se mettre en mode erreur
		//Si il y a des arbres adjacents mais qu'ils sont déjà associé, cf plus loin
		//le paramètre reserved_tree_xy_list liste les coordonnée des arbres volés, que les fonctions appelées ne peuvent donc pas revoler
		var assoc_tree_xy_list : Array[(Int, Int)] =Array()
		if (y != 0) {//la case n'est pas tout en haut
			val pot_tree = game_board(x)(y-1)
			if (pot_tree(0)==1){	//pot_tree est bien un arbre
				if (pot_tree(2) + pot_tree(3) == -2){	//arbre libre trouvé
					assoc(x,y,x,y-1)
					return(true)
				}
				else {assoc_tree_xy_list = assoc_tree_xy_list :+ (x,y-1)}	//arbre associé trouvé
			}

		}
		if (x != 0) {//la case n'est pas tout à gauche
			val pot_tree = game_board(x-1)(y)
			if (pot_tree(0)==1){	//pot_tree est bien un arbre
				if (pot_tree(2) + pot_tree(3) == -2){	//arbre libre trouvé
					assoc(x,y,x-1,y)
					return(true)
				}
				else {assoc_tree_xy_list = assoc_tree_xy_list :+ (x-1,y)}	//arbre associé trouvé
			}
		}
		if (x != nb_of_cols - 1) {//la case n'est pas tout à droite
			val pot_tree = game_board(x+1)(y)
			if (pot_tree(0)==1){	//pot_tree est bien un arbre
				if (pot_tree(2) + pot_tree(3) == -2){	//arbre libre trouvé
					assoc(x,y,x+1,y)
					return(true)
				}
				else {assoc_tree_xy_list = assoc_tree_xy_list :+ (x+1, y)}	//arbre associé trouvé
			}
		}
		if (y != nb_of_rows -1 ) {//la case n'est pas tout en bas
			val pot_tree = game_board(x)(y+1)
			if (pot_tree(0)==1){	//pot_tree est bien un arbre
				if (pot_tree(2) + pot_tree(3) == -2){	//arbre libre trouvé
					assoc(x,y,x,y+1)
					return(true)
				}
				else {assoc_tree_xy_list = assoc_tree_xy_list :+ (x,y+1)}	//arbre associé trouvé
			}
		}
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
						return(true)
					}
					else{	//l'autre tente n'a pas réussi à se trouver un autre arbre, on restore les associations et on continue avec l'arbre associé suivant
						unassoc(x,y)
						assoc(other_tent_x, other_tent_y, assoc_tree_xy._1, assoc_tree_xy._2)
					}
				}	
			}
			//cette tente ne peut s'associer à aucun arbre
			game_frame_content.grid.access_xy(x,y).set_no_adj_tree_error
			return(false)
		}

	}

	def add_tent(x:Int, y:Int) = {	//appelée par un label se changeant en tente à la case (x, y)
		if (game_board(x)(y)(1) == 0){}
		mod_game_board(x, y, tipe = 2, free=0)
		unfree_adjacent_squares(x,y)
		check_error(x, y)
		find_tree(x,y)
		check_win()
	}

	def restore_square_freedom(x:Int, y:Int) ={
		var adjacent_tent_nb = 0
		if (y > 0) {
			if(x > 0) {if(solved_board(x-1)(y-1)(0)==2){adjacent_tent_nb = adjacent_tent_nb+1}}				//top left
			if(solved_board(x)(y-1)(0)==2){adjacent_tent_nb = adjacent_tent_nb+1}							//top
			if(x < nb_of_cols-1){if(solved_board(x+1)(y-1)(0)==2){adjacent_tent_nb = adjacent_tent_nb+1}}	//top right
		}
		if(x > 0){if(solved_board(x-1)(y)(0)==2){adjacent_tent_nb = adjacent_tent_nb+1}}					//left
		if(x < nb_of_cols-1){if(solved_board(x+1)(y)(0)==2){adjacent_tent_nb = adjacent_tent_nb+1}}			//right
		if(y < nb_of_rows-1){
			if(x > 0){if(solved_board(x-1)(y+1)(0)==2){adjacent_tent_nb = adjacent_tent_nb+1}}				//bottom left
			if(solved_board(x)(y+1)(0)==2){adjacent_tent_nb = adjacent_tent_nb+1}							//bottom
			if(x < nb_of_cols-1){if(solved_board(x+1)(y+1)(0)==2){adjacent_tent_nb = adjacent_tent_nb+1}}	//bottom right
		}
		if (adjacent_tent_nb>0){}
		else{if (game_board(x)(y)(0) != 1) {mod_game_board(x, y, free=1)}}
	}

	def restore_adjacent_square_freedom(x:Int, y:Int) ={
		if (y > 0) {
			if(x > 0) {restore_square_freedom(x-1,y-1)}			//top left
			restore_square_freedom(x,y-1)						//top
			if(x < nb_of_cols-1){restore_square_freedom(x+1,y-1)}	//top right
		}
		if(x > 0){restore_square_freedom(x-1,y)}			//left
		if(x < nb_of_cols-1){restore_square_freedom(x+1,y)}		//right
		if(y < nb_of_rows-1){
			if(x > 0){restore_square_freedom(x-1,y+1)}		//bottom left
			restore_square_freedom(x,y+1)				//bottom
			if(x < nb_of_cols-1){restore_square_freedom(x+1,y+1)}	//bottom right
		}		
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
							mod_game_board(x,y,x_ass= -1,y_ass = -1 )
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
		unassoc(x,y)
		mod_game_board(x, y, tipe = 0, free=1)
		restore_adjacent_square_freedom(x,y)
		check_error(x,y)
		clear_no_assoc_tree_error()
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
	//turn change la couleur de la case (x,y) dans board
	/*
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
			case _ => println("anormal: la fonction maj_nb_of_moves de l'objet AngelWar a été appelée avec un argument différent de 1 ou 0:" + n)
		}
		val label_1 = game_frame_content.label_1
		label_1.text = "Retournements : " + nb_of_moves.toString
	}

	def check_win () ={
		if (nb_of_white_square == nb_of_rows * nb_of_cols && playing) {
			AngelWar.win()
		}
	}
	*/
}

/*
object Main {
	def main(args: Array[String]) {
		val ui = new UI(AngelWar)
		ui.visible = true
	}
}*/
