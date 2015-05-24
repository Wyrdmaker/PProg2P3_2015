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
import Games.Demineur._

package Games{
package Demineur{

//"DGE" -> "Demineur_Graphical_Element"
object DGE extends GUI_Graphical_Elements{
	def no_color_mode () = {
		//Le max est une sécurité. Si IndexOf ne trouve pas la chaine correspondant au mode de couleur dans la liste de ses valeurs possibles, il renvoie -1.
		//Ainsi, en cas de faute de frappe, le mode de couleur utilisé est le Normal
		max(0,Demineur.string_game_parameters_def_list(1)._3.indexOf(Demineur.string_game_parameters_def_list(1)._2))
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

	val demineur_color_list = List (
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

class Demineur_Help_Frame extends Frame {
	title = "Aide"
	contents = new Label("Allez, avouez que vous savez déjà jouer à ce jeu ! ;)"){
		background = GUI_Mood.b_colour
		foreground = GUI_Mood.f_colour
		opaque = true
	}
	visible = true
}

class Demineur_About_Frame extends Frame{
	title = "A Propos"
	contents = new Label("Interface Graphique par T.Dupriez et G.Hocquet"){
		background = GUI_Mood.b_colour
		foreground = GUI_Mood.f_colour
		opaque = true
	}
	visible = true
}

object Demineur extends Game{
	val title = "Démineur"

	val square_size_x = 30
	val square_size_y = 30
	var game_beginning_time: Date = null
	//var in_game = false héritée de Game

	//##Game parameters##
	var numeric_game_parameters_def_list = IndexedSeq(("Largeur", 0, 4, 25), ("Hauteur", 0, 4, 25), ("Mines", 0, 10, 10))
	var string_game_parameters_def_list = IndexedSeq(("Difficulté", "Facile", IndexedSeq("Facile", "Moyenne", "Difficile", "Libre")), ("Mode de Couleur", "Classique", IndexedSeq("Classique", "Creepy-Glauque", "RVB", "Automne", "Océan")), ("Mode Spectateur", "Désactivé", IndexedSeq("Joueur","Spectateur")))
	def nb_of_rows = numeric_game_parameters_def_list(1)._2  //fait de nb_of_rows un alias de la valeur du paramètre Height (ne marche que pour la lecture)
	def nb_of_cols = numeric_game_parameters_def_list(0)._2  //fait de nb_of_cols un alias de la valeur du paramètre Width (ne marche que pour la lecture)
	def nb_of_bombs = numeric_game_parameters_def_list(2)._2 //Ces deux fonctions réalisent un alias du champd valeur du 3ième paramètre numérique du Démineur
	def color_parameter = string_game_parameters_def_list(1)._2
	val has_numeric_parameters_0asWidth_1asHeight = true

	type Game_Label_Class = Demineur_Label
	def glb_factory () = { new Game_Label_Class } // "glb" -> "Game_Label_Class"
	type Game_Border_Label_Class = Label
	def gblb_factory () = { new Game_Border_Label_Class }
	def about_frame_factory () = { new Demineur_About_Frame }
	def help_frame_factory () = { new Demineur_Help_Frame }

	//var random_gen héritée de Game
	//var game_frame_content héritée de Game

	main_character_text_on_lose = main_character_text_on_lose ++ Array("Eeeeeet Boum !<br>J'adore les explosions !", "Ah non !<br>Ça, c'était une mine !")
	main_character_text_on_win = main_character_text_on_win ++ Array("Et un champ de mines nettoyé. Un !")
	main_character_text_on_launching = main_character_text_on_launching ++ Array("Le mode de difficulté \"Libre\" permet d'avoir une grille totalement aléatoire.","Ce jeu me rappelle la fois où j'ai tué mon clone.<br>Hi, hi, hi, c'était vraiment trop bien !","Tu pense que ce jeu est inclus dans la formation des vrais démineurs ?","Oooh, qu'il est mignon ce champ de mines !")

	val game_game_mode_list = IndexedSeq(
		Game_Mode(IndexedSeq(9, 9, 10),IndexedSeq("Facile", "Classique", "Joueur")),
		Game_Mode(IndexedSeq(16, 16, 40),IndexedSeq("Moyenne", "Classique", "Joueur")),
		Game_Mode(IndexedSeq(16, 16, 65),IndexedSeq("Difficile", "Classique", "Joueur")),
		Game_Mode(IndexedSeq(25, 25, 150),IndexedSeq("Difficile", "Classique", "Joueur")),
		Game_Mode(IndexedSeq(9, 9, 10),IndexedSeq("Facile", "Classique", "Spectateur")),
		Game_Mode(IndexedSeq(16, 16, 40),IndexedSeq("Moyenne", "Classique", "Spectateur")),
		Game_Mode(IndexedSeq(16, 16, 65),IndexedSeq("Difficile", "Classique", "Spectateur"))
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
		Demineur.nb_discovered_square = 0
		Demineur.nb_flagged_square = 0
		Demineur.maj_nb_flag(0)

		//game_frame_content.bottom_panel.background = DGE.bottom_panel_color_list(DGE.no_color_mode)
		game_frame_content.grid.get_contents.foreach(label => label.init())
	}
	def game_action_restart() : Unit = {
		val grid_contents = Demineur.game_frame_content.grid.get_contents
		grid_contents.foreach(label => label.init())

		Demineur.nb_discovered_square = 0
		Demineur.nb_flagged_square = 0
		Demineur.maj_nb_flag(0)
	}
	//Définit ce qui se passe en cas de victoire du joueur -> voir Game
	override def win() = {
		super.win()		
	}
	//Définit ce qui se passe en cas de défaite du joueur -> voir Game
	override def lose() = {
		super.lose()
	}

	//##Demineur Variables## // Variables internes au Démineur
	var nb_discovered_square = 0
	var nb_flagged_square = 0

	//##Demineur Functions## //Fonctions internes au Démineur

	//Incremente le nombres de cases découvertes et déclenche éventuellement la victoire
	def increment_nb_discovered_square() = {
		nb_discovered_square += 1
		if (nb_discovered_square + nb_of_bombs == nb_of_rows * nb_of_cols)
			win()
	}

	//A un numéro de case, associe la liste des numéros des cases adjacentes(en faisant attention aux bords de la grille)
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

	//Met à jour le nombre de cases marquées par un drapeau en accord avec son argument. Met à jour le label du nombre de drapeaux de la fenetre (label_1)
	def maj_nb_flag(n : Int /*normalement 1, -1 ou 0*/) = {
		n match {
			case 1 => nb_flagged_square = nb_flagged_square + n 
			case -1 => nb_flagged_square = nb_flagged_square + n
			case 0 => nb_flagged_square = nb_flagged_square + n
			case _ => println("anormal: la fonction maj_nb_flag de l'objet Demineur a été appelée avec un argument différent de 1, -1 ou 0:" + n)
		}
		val label_1 = game_frame_content.label_1
		label_1.text = "Mines : " + nb_flagged_square.toString + " / " + nb_of_bombs.toString
		if (nb_flagged_square > nb_of_bombs)
			label_1.foreground = DGE.red
		else
			label_1.foreground = DGE.dark_golden_rod1
	}

	def debug_mode ():Boolean={
		//"debug_mode" = "Mode Spectateur"
		string_game_parameters_def_list(2)._2==("Spectateur")
	}

	//Est appelée lors du premier clic sur un label.
	//Place les bombes parmi les labels de la grille (autre que le label cliqué et ses 8 voisins).
	//Indique ensuite à chaque label (autre que ceux contenant une bombe) le nombre de ses voisins contenant une bombe -> label.value
	def place_bombs(n_origin_label : Int) = {
		case class Interruption_in_Debug_Mode(message:String) extends Throwable{}
		def place_bombs_action()={

			val grid = game_frame_content.grid
			var bombs_left = nb_of_bombs

			var game_board: Array[String] = Array()
			//Tableau stockant les valeurs des cases
			//la valeur d'une case est: - "?" pour les cases non initialisées
			//							- "#" pour les cases non initialisées ne devant pas recevoir de bombes (celles adjacentes à la première case cliquée)
			//							- "b" si la case contient une bombe
			//							- "n" où n est le nombre de bombes adjacentes (diagonales incluses)
			def generate_a_game_board () ={
				if(debug_mode()){println("Generation")}
				//Remplit game_board en plaçant les bombes et en calculant les valeurs des cases
				bombs_left = nb_of_bombs
				game_board = Array.fill(nb_of_cols*nb_of_rows){("?")}
				neighbour(n_origin_label).foreach(n => game_board(n) = "#")
				while (bombs_left > 0) {
					var random = random_gen.nextInt(nb_of_rows * nb_of_cols)
					if (game_board(random) == "?") {
						game_board(random) = "b"
						bombs_left -= 1
					}
				}
				
				for (n <- 0 to (nb_of_rows*nb_of_cols-1)){
					val value = game_board(n)
					if(value != "b"){
						var new_value = 0
						neighbour(n).foreach(number =>{
							if (game_board(number) == "b") {new_value +=1}
						})
						game_board(n) = new_value.toString
					}
				}		
			}
			val difficulty:Int = string_game_parameters_def_list(0)._2 match{
				case "Facile" => 0
				case "Moyenne" => 1
				case "Difficile" => 2
				case "Libre" => 3
				case _ => {println("Attention: Mode dé difficulé non reconnu");3}
			}
			def apply_game_board() ={
				//Affecte aux labels de la grille les valeurs qu'ils ont dans game_board
				game_frame_content.grid.get_contents.foreach(label => label.value = game_board(label.numero))
			}

			def solve_game_board ():Boolean ={
				//Essaye de résoudre la partie définit par game_board et renvoit true si réussite et false si échec

				if(debug_mode()){
					println("Résolution débutée")
				 	Larissa.say_smth(Array("Un mode spectateur pour un démineur !!!<br>Et pourquoi pas une fonction pour conserver ses replays tant qu'on y est ?",
						"Je parie que je peux résoudre cette grille avant le solveur !",
						"Tu pourrais pas coder d'autres jeux au lieu de regarder ton ordinateur jouer au démineur ?"))
				 }

				val deduction_board: Array[Int] = Array.fill(nb_of_cols*nb_of_rows){0}
				//Tableau stockant les connaissances du solveur sur le game_board 
				//Pour chaque case: 1 => Le solveur connait la valeur de la case
				//					0 => Le solveur ignore la valeur de la case

				val unknown_neighbours_board: Array[Int] = Array.fill(nb_of_cols*nb_of_rows){0}
				//Tableau stockant le nombre de cases voisines inconnues de chaque case

				for (n <- 0 to (nb_of_rows*nb_of_cols-1)){unknown_neighbours_board(n)=(neighbour(n)).length}
				//Remplissage du tableau unknown_neighbours_board

				var nb_of_found_bombs = 0
				val knowledge_frontier: scala.collection.mutable.Set[Int] = collection.mutable.Set()
				//Stocke les numéros des cases connues du solveur tq au moins une case voisine (diagonales incluses) soit inconnue

				val rule_easy_1_application_board: Array[Boolean] = Array.fill(nb_of_cols*nb_of_rows){false}
				//tableau stockant pour chaque case si la règle easy 1 a été appliquée dessus

				val rule_easy_2_application_board: Array[Boolean] = Array.fill(nb_of_cols*nb_of_rows){false}
				//tableau stockant pour chaque case si la règle easy 2 a été appliquée dessus

				val premise_board: Array[Boolean] = Array.fill(nb_of_rows*nb_of_cols){false}
				//tableau stockant pour chaque case si c'est actuellement une prémisse ou non (sert uniquement dans un but graphique)

				for (i <- 0 to (nb_of_rows*nb_of_cols-1)){
					//Sert à empécher d'essayer d'appliquer les règles faciles 1 et 2 sur des cases de bombes
					if(game_board(i)=="b"){
						rule_easy_1_application_board(i)=true
						rule_easy_2_application_board(i)=true
					}
				}
				//true si le solveur a réussi à trouver les mines
				var is_game_solved = false
				//Sert dans la boucle de résolution, vrai si l'itération a découvert une nouvelle case
				var progression = false
				//Sert uniquement au mode debug/spectateur (pour laisser le solveur finir la grille en cours)
				var finish =false

				//Ne plus déclarer de val/var non-encapsulées à partir d'ici. Sinon, ça ne compile pas: "forward reference extends over definition of val/var ..."
				def kf_add(n:Int)={knowledge_frontier += n; if(debug_mode){set_kf_border(n)}}
				def kf_rem(n:Int)={knowledge_frontier -= n; if(debug_mode){set_default_border(n)}}
				def set_kf_border(n:Int)={if(!premise_board(n)){game_frame_content.grid.access_n(n).debug_set_blue_border()}}
				def set_discovery_border(n:Int)={game_frame_content.grid.access_n(n).debug_set_purple_border()}
				def set_premise_border(n:Int)={game_frame_content.grid.access_n(n).debug_set_cyan_border()}
				def rem_premise_border(n:Int)={if(knowledge_frontier(n)){set_kf_border(n)}else{set_default_border(n)}}

				def known(n:Int):Boolean={deduction_board(n)==1}
				//Renvoie vrai ssi la case n est connue du solveur
				def add_knowledge(n:Int)={deduction_board(n)=1}
				//Change le contenu de deduction_board pour refléter le fait que le solveur ait déduit une nouvelle case
				def value(n:Int):String={game_board(n)}
				//renvoie la valeur d'une case

				def set_default_border(n:Int)={game_frame_content.grid.access_n(n).debug_set_black_border()}
				def discovered(n_discovered_square:Int)={
					//Fonction à appeler lorsque le solveur a déterminé la valeur d'une case
					if(!known(n_discovered_square)){
						if(debug_mode){
							set_discovery_border(n_discovered_square)
							debug_stop("Découverte ("+ n_discovered_square+")")
							game_frame_content.grid.access_n(n_discovered_square).debug_reveal()
						}
						add_knowledge(n_discovered_square)
						neighbour(n_discovered_square).foreach(n => {
							unknown_neighbours_board(n)-=1
							if (unknown_neighbours_board(n)<=7 && known(n)){kf_add(n)}
							if (unknown_neighbours_board(n)<=0 && known(n)){kf_rem(n)}
						})
						if(unknown_neighbours_board(n_discovered_square)<=7){kf_add(n_discovered_square)}
						if(unknown_neighbours_board(n_discovered_square)<=0){kf_rem(n_discovered_square)}
						spread_knowledge(n_discovered_square)
					}
				}
				def spread_knowledge(n:Int):Unit={
					//Appliqué sur un label connu du solveur et ayant une valeur de 0, découvre les cases adjacentes et récursivement si les valeurs des cases dévoilées sont 0
					if(known(n) && game_board(n)=="0"){
						neighbour(n).foreach(m => {if(!known(m)){discovered(m)/*;spread_knowledge(m)*/}})
					}
				}
				//Découvre la case où le jouer a cliqué et ses voisines
				discovered(n_origin_label)
				spread_knowledge(n_origin_label)

				def rule_easy_1 (n:Int):List[(Int,Boolean)]={
					/*Essaie d'appliquer la règle de déduction:
					"Lorsque le nombre de voisins non-dévoilés est égal à la valeur de la case (- le nombre de voisins mines dévoilés), tout les voisins sont des mines"
					Renvoie un tableau contenant les découvertes réalisées (format: (n°case,bombe?))
					*/
					if(rule_easy_1_application_board(n)==true){println("Attention: on a appliqué rule_easy_1 sur une case qui en avait déjà fait l'objet")}
					rule_easy_1_application_board(n)=true
					var nb_of_known_bomb_neighbours=0
					val unknown_neighbours = neighbour(n).filter(m => {if((game_board(m)=="b")&&known(m)){nb_of_known_bomb_neighbours+=1};!known(m)})
					var return_value: List[(Int,Boolean)] = List()
					if(unknown_neighbours.length==(value(n).toInt - nb_of_known_bomb_neighbours)){
						return_value = unknown_neighbours.map(o => (o,true))
						//unknown_neighbours.foreach(o => return_value += (o,true))
					}
					return return_value
				}

				def rule_easy_2 (n:Int):List[(Int,Boolean)]={
					/*Essaie d'appliquer la règle de déduction:
					"Lorsque le nombre de mines connues parmi les voisins est égal au numéro de la case, tout les voisins dont des mines"
					Renvoie un tableau contenant les découvertes réalisées (format: (n°case,bombe?))
					*/
					if(rule_easy_2_application_board(n)==true){println("Attention: on a appliqué rule_easy_2 sur une case qui en avait déjà fait l'objet")}
					rule_easy_2_application_board(n)=true
					var return_value: List[(Int,Boolean)] = List()
					val neighbours = neighbour(n)
					val unknown_neighbours = neighbours.filter(m => (!known(m)))
					val nb_of_known_bomb_neighbours = neighbours.filter(m => ((known(m))&&(value(m)=="b"))).length
					if(nb_of_known_bomb_neighbours==value(n).toInt){
						return_value = unknown_neighbours.map(o => (o,false))
						//unknown_neighbours.foreach(o => return_value += (o,false))
					}
					return return_value
				}

				def rule_medium_1 (n1:Int,n2:Int):List[(Int,Boolean)]={
					/*Essaie d'appliquer la règle de déduction:
					"Soient V1,V2 les voisins non révélés de n1 et n2. Soient m1 et m2 les valeurs de n1 et n2
					-Lorsque V1 [inter] V2 [différent de] [ensemble vide] et que |V1\V2|=m1-m2, alors V1\V2 ne comporte que des mines et V2\V1 ne comporte aucune mine
					-Lorsque V1 [inclut dans] V2, le nombre de mines dans V2\V1 est égal à m2-m1
					*/				
					var result:List[(Int,Boolean)] =List()

					//if(debug_mode){println("n1=" + n1 + ", n2=" + n2)}
					val n1_neighbours = neighbour(n1)
					val v1 = n1_neighbours.filter(m => (!known(m)))
					//if(debug_mode){println("voisins inconnus de n1 = " + v1)}
					if(v1.length==0){return List() }

					val n2_neighbours = neighbour(n2)
					val v2 = n2_neighbours.filter(m => (!known(m)))
					//if(debug_mode){println("voisins inconnus de n2 = " + v2)}
					if(v2.length==0){return(List())}

					val v1_int_v2 = v1.intersect(v2)
					//if(debug_mode){println("voisins inconnus dans l'intersection = " + v1_int_v2)}
					
					val m1=value(n1).toInt - n1_neighbours.filter(m => ((known(m))&&(value(m)=="b"))).length
					val m2=value(n2).toInt - n2_neighbours.filter(m => ((known(m))&&(value(m)=="b"))).length

					if(v1_int_v2.length > 0){
						val v1_without_v2 = v1.diff(v2)
						val v2_without_v1 = v2.diff(v1)
						val v1_without_v2_l = v1_without_v2.length
						val v2_without_v1_l = v2_without_v1.length
						//Premier tiret de la règle
						if(v1_without_v2_l == m1 - m2){
							result = result ::: v1_without_v2.map(m => (m,true))
							result = result ::: ((v2_without_v1).map(m => (m,false)))
						}
						//if(debug_mode){println("res1="+result)}
						//La règle symétrique du premier tiret de la règle
						if(v2_without_v1_l == m2 - m1){
							result = result ::: v2_without_v1.map(m => (m,true))
							result = result ::: ((v1_without_v2).map(m => (m,false)))
						}
						//if(debug_mode){println("res2="+result)}
						//Deuxième tiret de la règle
						if(v1_without_v2_l==0){
							if(m2-m1==0){
								result = result ::: (v2_without_v1.map(m => (m,false)))
							}
							else{
								if(v2_without_v1_l==m2-m1){
									result = result ::: (v2_without_v1.map(m => (m,true)))
								}
							}
							
						}
						//if(debug_mode){println("res3="+result)}
						//la règle symétrique du deuxième tiret de la règle
						if(v2_without_v1_l==0){
							if(m1-m2==0){
								result = result ::: (v1_without_v2.map(m => (m,false)))
							}
							else{
								if(v1_without_v2_l==m1-m2){
									result = result ::: (v1_without_v2.map(m => (m,true)))
								}
							}
							
						}
						//if(debug_mode){println("res4="+result)}						

					}
					return (result.distinct)
				}

				def next_square_for_rule_easy_1 ():Int={
					//renvoie la première case sur laquelle on puisse appliquer rule_easy_1. Renvoie -1 si une telle case n'existe pas
					knowledge_frontier.find(n => !rule_easy_1_application_board(n)) match{
						case Some(m) => m
						case None => -1
					}
				}
				def next_square_for_rule_easy_2 ():Int={
					//renvoie la première case sur laquelle on puisse appliquer rule_easy_2. Renvoie -1 si une telle case n'existe pas
					knowledge_frontier.find(n => !rule_easy_2_application_board(n)) match{
						case Some(m) => m
						case None => -1
					}
				}

				def check_inference(n_inferred_square:Int,bomb_inference:Boolean):Boolean={
					//Vérifie une déduction d'une règle en comparant avec le game_board
					if(!((game_board(n_inferred_square)=="b")==bomb_inference)){
						//Déduction fausse
						return false
					}
					return true
				}

				def handle_rule_discovery(n_discovered_square:Int,bomb_infered:Boolean)={
					//Gére une découverte de case en:
					//- indiquant dans les tableaux rule_easy_1_application_board et rule_easy_2_application_board que la case découverte peut de nouveau être utilisée par ces règles
					//- incrémentant le nb de bombes découvertes si pertinent
					//- appelant la fonction discovered sur la case découverte
					if(debug_mode){
						if(known(n_discovered_square) && value(n_discovered_square)=="b"){
							println("La bombe en case " + n_discovered_square + " avait déjà été découverte")
						}
					}
					neighbour(n_discovered_square).foreach(n => {
						if(game_board(n)!="b"){
							//on a découvert cette case
							rule_easy_1_application_board(n)=false
							rule_easy_2_application_board(n)=false
						}
					})
					if(game_board(n_discovered_square)=="b"){
						nb_of_found_bombs+=1
						if(debug_mode){/*Graphique*/maj_nb_flag(1)}
						if(nb_of_found_bombs==nb_of_bombs){is_game_solved=true}
					}
					discovered(n_discovered_square)
				}
				//Graphique
				def add_premise(n:Int)={
					premise_board(n)=true
					set_premise_border(n)
				}
				//Graphique
				def rem_premise(n:Int)={
					premise_board(n)=false
					rem_premise_border(n)
				}

				//Applique les règles easy 1 et easy 2 tant que c'est possible
				def easy_rules_loop()={
					while(((next_square_for_rule_easy_1()!= -1)||(next_square_for_rule_easy_2()!= -1))&& !is_game_solved){

						while(next_square_for_rule_easy_1() != -1){
							val premise = next_square_for_rule_easy_1()
							val rule_result = rule_easy_1(premise)
							if(rule_result.length != 0){
								//La règle a déduit quelque chose
								progression=true
								if(debug_mode){
									println("règle facile 1")
									println(rule_result)
									add_premise(premise)									
								}
								rule_result.foreach(n_case_bomb_inference =>{
									if(!check_inference(n_case_bomb_inference._1,n_case_bomb_inference._2)){
										//Déduction fausse
										if(debug_mode){
											println("La règle facile 1 s'est trompée dans la déduction de la case ("+n_case_bomb_inference._1%nb_of_cols+","+n_case_bomb_inference._1/nb_of_cols+")")
										}
									}
									handle_rule_discovery(n_case_bomb_inference._1,n_case_bomb_inference._2)
								})
								if(debug_mode){rem_premise(premise)}					
							}
						}
						while(next_square_for_rule_easy_2()!= -1){
							val premise = next_square_for_rule_easy_2()
							val rule_result = rule_easy_2(premise)
							if(rule_result.length != 0){
								//La règle a déduit quelque chose
								progression=true
								if(debug_mode){
									println("règle facile 2")
									println(rule_result)
									add_premise(premise)									
								}

								rule_result.foreach(n_case_bomb_inference =>{
									if(!check_inference(n_case_bomb_inference._1,n_case_bomb_inference._2)){
										//Déduction fausse
										if(debug_mode){
											println("La règle facile 2 s'est trompée dans la déduction de la case ("+n_case_bomb_inference._1%nb_of_cols+","+n_case_bomb_inference._1/nb_of_cols+")")
										}
									}
									handle_rule_discovery(n_case_bomb_inference._1,n_case_bomb_inference._2)
								})
								if(debug_mode){rem_premise(premise)}				
							}
						}
					}
				}

				def medium_rules_1_everywhere()={
					//Applique la règle medium 1 tant que c'est possible
					//on applique medium_rule_1 sur les couples (n1,n2) tel que 
					//les deux soient dans la knowledge_frontier, que n1<n2, que n2 soit "à portée" de n1 (pour qu'ils puissent avoir des voisins communs)
					knowledge_frontier.foreach(n1 => knowledge_frontier.foreach(n2 =>{
							var right_limit=n1 + min(2,nb_of_cols-n1%nb_of_cols-1)
							var left_limit=n1 - min(2,n1%nb_of_cols)
							if((value(n1)!="b")&&(value(n2)!="b")&&(n1 < n2)&&((n2<=right_limit)||((left_limit+nb_of_cols<=n2)&&(n2<=right_limit+nb_of_cols))||((left_limit+2*nb_of_cols<=n2)&&(n2<=right_limit+2*nb_of_cols)))){
								val rule_result = rule_medium_1(n1,n2)
								if(rule_result.length != 0){
									//La règle a déduit quelque chose
									progression=true
									if(debug_mode){
										println("règle medium 1")
										println(rule_result)
										add_premise(n1)
										add_premise(n2)									
									}
									rule_result.foreach(n_case_bomb_inference =>{
										if(!check_inference(n_case_bomb_inference._1,n_case_bomb_inference._2)){
											//Déduction fausse
											if(debug_mode){
												println("La règle medium 1 s'est trompée dans la déduction de la case ("+n_case_bomb_inference._1%nb_of_cols+","+n_case_bomb_inference._1/nb_of_cols+")")
											}
										}
										handle_rule_discovery(n_case_bomb_inference._1,n_case_bomb_inference._2)
									})
									if(debug_mode){rem_premise(n1);rem_premise(n2)}					
								}
							}
						}
					))
				}

				def rule_hard_1():(List[(Int,Boolean)],scala.collection.immutable.Set[Int])={
					//Si il existe c1,..cn tq l'ensemble de leurs voisins non déterminés soient disjoint deux à deux et que la somme de leur nombre
					//de mines non déterminées est égale au nombre restant de mines à trouver, alors toutes les cases hors de ces voisins n'ont pas de mines
					//Renvoie le couple (déductions de la règle, ensemble des c1,..cn)
					
					if(debug_mode){println("essai de rule hard 1")}
					val nb_unknown_bombs = nb_of_bombs - nb_of_found_bombs
					var final_unknow_neighbours_set: scala.collection.immutable.Set[Int]= scala.collection.immutable.Set()
					def f_unknown_neighbour_set(n:Int)={neighbour(n).filter(m => !known(m))}
					def find_set(nb_captured_bombs:Int, c_set: scala.collection.immutable.Set[Int], unknown_neighbours_set: scala.collection.immutable.Set[Int], choosable_squares_set: scala.collection.immutable.Set[Int]):Option[(scala.collection.immutable.Set[Int],scala.collection.immutable.Set[Int])]={
						//renvoie le couple (ensemble des voisins inconnus des c1,..cn;ensemble des c1,..cn)
						//Essaie récursivement d'ajouter des éléments de choosable_squares_set à c_set pour capturer toutes les mines restantes
						if(nb_captured_bombs==nb_unknown_bombs){return(Some(unknown_neighbours_set,c_set))}
						else{
							if(choosable_squares_set.size!=0){
								choosable_squares_set.foreach(ch =>{
									val ch_neighbours = neighbour(ch)
									val ch_known_neighbours = ch_neighbours.filter(m=>known(m))
									val ch_unknown_neighbours = ch_neighbours.filter(m=> !known(m))
									val additional_bombs_captured = value(ch).toInt-ch_known_neighbours.filter(m => value(m)=="b").length
									find_set(nb_captured_bombs+additional_bombs_captured,
											c_set + ch,unknown_neighbours_set++(ch_unknown_neighbours),
											((choosable_squares_set-ch)--ch_known_neighbours)--(ch_unknown_neighbours.map(n=>(neighbour(n)).filter(m=>known(m)))).flatten) match{
										case Some((u_set,c_set))=>return(Some(u_set,c_set))
										case None => return(None)
									}
								})
								return(None)
							}
							else{return(None)}
						}

					}
					find_set(0, scala.collection.immutable.Set(), scala.collection.immutable.Set(), scala.collection.immutable.Set()++(knowledge_frontier.filter(n => value(n)!="b"))) match{
						case Some((u_set,c_set))=>{
							val unknown_squares = (List.range(0,nb_of_cols*nb_of_rows,1)).filter(m => !known(m))
							return (((unknown_squares.filter(n => !u_set(n))).map(n => (n,false)),c_set))
						}
						case None => return((List(),scala.collection.immutable.Set()))
					}
				}

				//Essaie d'appliquer la règle hard 1 tant que c'est possible
				def rule_hard_1_loop()={
					var rule_hard_1_yields = true
					while(rule_hard_1_yields){
						val rule_result = rule_hard_1()
						val deduction_list=rule_result._1
						val c_set=rule_result._2
						if(deduction_list.length != 0){
							//La règle a déduit quelque chose
							progression=true
							if(debug_mode){
								println("règle hard 1")
								println(deduction_list)
								c_set.foreach(n => add_premise(n))
																	
							}
							deduction_list.foreach(n_case_bomb_inference =>{
								if(!check_inference(n_case_bomb_inference._1,n_case_bomb_inference._2)){
									//Déduction fausse
									if(debug_mode){
										println("La règle hard 1 s'est trompée dans la déduction de la case ("+n_case_bomb_inference._1%nb_of_cols+","+n_case_bomb_inference._1/nb_of_cols+")")
									}
								}
								handle_rule_discovery(n_case_bomb_inference._1,n_case_bomb_inference._2)
							})
							if(debug_mode){c_set.foreach(n => rem_premise(n))}					
						}
						else(rule_hard_1_yields=false)
					}
				}


				progression=true
				difficulty match{
					case 0 => 	easy_rules_loop()
					case 1 => 	while(!is_game_solved&&progression){
														progression=false
														easy_rules_loop()
														medium_rules_1_everywhere()
								}
					case 2 => 	while(!is_game_solved&&progression){
														progression=false
														easy_rules_loop()
														medium_rules_1_everywhere()
														rule_hard_1_loop()
								}
					case _ => {println("Solveur désactivé");is_game_solved=true}
				}

				//Sert à faire des arrêts dans la résolution (en mode debug/spectateur seulement)
				def debug_stop(message:String):Unit ={
					if(debug_mode&& !finish){
						println(message + "  (Entrer pour continuer/f pour dérouler/Ctrl-C pour quitter)")
						val line = Console.readLine
						if(line=="q"){/*throw new Interruption_in_Debug_Mode("Interruption_in_Debug_Mode")*/ System.exit(0)}
						if(line=="kf"){
							println("x?")
							val x = Console.readLine.toInt
							println("y?")
							val y = Console.readLine.toInt
							println(knowledge_frontier(x + y*nb_of_cols))
							debug_stop("Autre chose ?")}
						if(line=="bf"){println("Number of bombs found =" + nb_of_found_bombs);debug_stop("Autre chose ?")}
						if(line=="f"){finish=true/*Pour que le solveur finisse tout seul la grille en cours*/}
					}
				}
				
				if(true){
					if(is_game_solved){
						finish=false
						println("Difficulté: " + difficulty)
						if(debug_mode){Larissa.say_smth(Array("Peuh, j'avais déjà trouvé depuis longtemps !","Et maintenant, on fait quoi ?", "Ah ben c'est pas trop tôt !<br>J'ai eu le temps de mettre une raclée à Ed ! TROIS FOIS !<br>Ce gamin est mauvais !"))}
						debug_stop("Résolution réussie")
					}
					else {
						finish=false
						println("Difficulté: " + difficulty)
						if(debug_mode){Larissa.say_smth(Array("Il s'arrète déjà ?","Comment fais ce solveur pour être aussi nul !<br>C'était pourtant facile.","Et allez ! C'est reparti pour un tour !","Tu veux pas faire autre chose que de regarder ce solveur se planter ?"))}
						debug_stop("Résolution échouée")
					}
				}
				return(is_game_solved)
			}

			var nb_of_game_creation_tries = 0

			//Choisissez l'un des deux modes suivant: 
			//##BOUNDED MODE##
			val nb_of_game_creation_tries_limit = 1000
			//##ENDLESS MODE##
			//def nb_of_game_creation_tries_limit = nb_of_game_creation_tries + 1

			var game_solved = false
			if(debug_mode()){println("Difficulté: " + difficulty)}
			while((nb_of_game_creation_tries < nb_of_game_creation_tries_limit) && !game_solved){
				//Graphique
				nb_flagged_square=0
				//Graphique
				maj_nb_flag(0)
				println("Boucle Création-Résolution, essai "+(nb_of_game_creation_tries + 1))
				generate_a_game_board()
				if(debug_mode){
					game_frame_content.grid.get_contents().foreach(label => label.debug_hide())
					apply_game_board()
				}
				game_solved = solve_game_board()

				nb_of_game_creation_tries += 1
			}
			if(nb_of_game_creation_tries==nb_of_game_creation_tries_limit){println("Nombre maximum de tentatives de création-résolution atteint, instance aléatoire proposée")}
			apply_game_board()
		}
		/*var interruption_in_debug_mode_launched = false
		var game_creation_thread = new Thread {
			override def run {
				try {
					place_bombs_action()					
				}	catch {
					case e: Interruption_in_Debug_Mode => {
						println("Interruption on Debug Mode")
						interruption_in_debug_mode_launched = true
					}
				}

			}
		}*/

		/*On ne lance le solveur dans un autre thread que si le mode debug/spectateur est activé (permet de laisser le thread principal à swing pour qu'il
			puisse s'occuper de la partie graphique(si on ne fait pas ça, les debug_stop du solveur empéchent swing de faire quoi que ce soit))*/
		var game_creation_thread = new Thread {
			override def run {
				place_bombs_action()					
			}
		}

		if(debug_mode){
			game_frame_content.grid.get_contents.foreach(label => label.debug_deaf_to_mouse())
			game_frame_content.grid.access_n(n_origin_label).debug_set_black_border()
			//##

			game_creation_thread.start()
			println("thread start")
			//##
		}
		else{place_bombs_action()}
	}
	//Un label qui se découvre avec une valeur égale à 0 (ie aucun de ses voisins ne contient de bombes) appelle cette fonction pour que ses voisins se découvrent
	def spread(numero : Int) = {
		val grid_content = game_frame_content.grid.get_contents
		var voisins_list = neighbour(numero)
		voisins_list.foreach(numero => grid_content(numero).discover())
		
	}
}

}	//accolade fermante du package Demineur
}	//accolade fermante du package Games