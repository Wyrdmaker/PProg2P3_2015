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
	var string_game_parameters_def_list = IndexedSeq(("Difficulté", "Facile", IndexedSeq("Facile", "Moyenne", "Difficile", "Absurde")), ("Mode de Couleur", "Classique", IndexedSeq("Classique", "Creepy-Glauque", "RVB", "Automne", "Océan")), ("Mode Spectateur", "Désactivé", IndexedSeq("Désactivé","Activé")))
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

	val game_game_mode_list = IndexedSeq(
		Game_Mode(IndexedSeq(9, 9, 10),IndexedSeq("Facile", "Classique", "Désactivé")),
		Game_Mode(IndexedSeq(16, 16, 40),IndexedSeq("Moyenne", "Classique", "Désactivé")),
		Game_Mode(IndexedSeq(16, 16, 99),IndexedSeq("Difficile", "Classique", "Désactivé")),
		Game_Mode(IndexedSeq(9, 9, 10),IndexedSeq("Facile", "Classique", "Activé")),
		Game_Mode(IndexedSeq(16, 16, 40),IndexedSeq("Moyenne", "Classique", "Activé")),
		Game_Mode(IndexedSeq(16, 16, 99),IndexedSeq("Difficile", "Classique", "Activé"))
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
		string_game_parameters_def_list(2)._2==("Activé")
	}

	//Est appelée lors du premier clic sur un label.
	//Place les bombes parmi les labels de la grille (autre que le label cliqué et ses 8 voisins).
	//Indique ensuite à chaque label (autre que ceux contenant une bombe) le nombre de ses voisins contenant une bombe -> label.value
	def place_bombs(n_origin_label : Int) = {
		case class Interruption_in_Debug_Mode(message:String) extends Throwable{}
		def place_bombs_action()={

			val grid = game_frame_content.grid
			var bombs_left = nb_of_bombs 

			//Ancienne position de la fonction debug_stop

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
				case "Absurde" => 3
			}
			def apply_game_board() ={
				//Affecte aux labels de la grille les valeurs qu'ils ont dans game_board
				game_frame_content.grid.get_contents.foreach(label => label.value = game_board(label.numero))
			}

			def solve_game_board ():Boolean ={
				//Essaye de résoudre la partie définit par game_board et renvoit true si réussite et false si échec

				if(debug_mode()){println("Résolution débutée")}

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
				val rule_medium_1_application_board: Array[Array[Boolean]] = Array.fill(nb_of_cols*nb_of_rows){Array.fill(nb_of_cols*nb_of_rows){false}}
				//tableau stockant pour chaque couple de case si la règle medium 1 a été appliquée dessus 
				//(on accéde à une case du tableau en utilisant les numéros de case du couple dans l'ordre croissant)
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
				//Sert dans la boucle de résolution, vrai si l'itération a découvert une case
				var progression = false

				//Ne plus déclarer de val/var non-encapsulées à partir d'ici. Sinon, ça ne compile pas: "forward reference extends over definition of val/var ___"
				def kf_add(n:Int)={knowledge_frontier += n; if(debug_mode){set_kf_border(n)}}
				def kf_rem(n:Int)={knowledge_frontier -= n; if(debug_mode){set_default_border(n)}}
				def set_kf_border(n:Int)={if(!premise_board(n)){game_frame_content.grid.access_n(n).debug_set_blue_border()}}
				def set_discovery_border(n:Int)={game_frame_content.grid.access_n(n).debug_set_purple_border()}
				def set_premise_border(n:Int)={game_frame_content.grid.access_n(n).debug_set_cyan_border()}
				def rem_premise_border(n:Int)={if(knowledge_frontier(n)){set_kf_border(n)}else{set_default_border(n)}}

				def known(n:Int):Boolean={deduction_board(n)==1}
				//Renvoie vrai si la case n est connue du solveur
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
							//game_frame_content.grid.access_n(n_discovered_square).debug_set_purple_border()
							debug_stop("Découverte ("+ (n_discovered_square%nb_of_cols)+","+(n_discovered_square/nb_of_cols)+")")
							game_frame_content.grid.access_n(n_discovered_square).debug_reveal()
							//if(knowledge_frontier(n_discovered_square)){set_kf_border(n_discovered_square)}
						}
						add_knowledge(n_discovered_square)
						neighbour(n_discovered_square).foreach(n => {
							unknown_neighbours_board(n)-=1
							if (unknown_neighbours_board(n)<=7 && known(n)){kf_add(n)/*knowledge_frontier += n*/}
							if (unknown_neighbours_board(n)<=0 && known(n)){kf_rem(n)/*knowledge_frontier -= n*/}
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
				discovered(n_origin_label)
				spread_knowledge(n_origin_label)

				def rule_easy_1 (n:Int):List[(Int,Boolean)]={
					//rule_number = 01 USELESS
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
					//rule number = 02 USELESS
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
					//if(rule_medium_1_application_board(n1)(n2)==true){println("Attention: on a appliqué rule_medium_1 sur un couple qui en avait déjà fait l'objet")}
					//rule_medium_1_application_board(n1)(n2)=true					
					var result:List[(Int,Boolean)] =List()

					if(debug_mode){println("n1=" + n1 + ", n2=" + n2)}
					val n1_neighbours = neighbour(n1)
					val v1 = n1_neighbours.filter(m => (!known(m)))
					if(debug_mode){println("voisins inconnus de n1 = " + v1)}
					if(v1.length==0){return List() }

					val n2_neighbours = neighbour(n2)
					val v2 = n2_neighbours.filter(m => (!known(m)))
					if(debug_mode){println("voisins inconnus de n2 = " + v2)}
					if(v2.length==0){return(List())}

					val v1_int_v2 = v1.intersect(v2)
					if(debug_mode){println("voisins inconnus dans l'intersection = " + v1_int_v2)}
					
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
						if(debug_mode){println("res1="+result)}
						//La règle symétrique
						if(v2_without_v1_l == m2 - m1){
							result = result ::: v2_without_v1.map(m => (m,true))
							result = result ::: ((v1_without_v2).map(m => (m,false)))
						}
						if(debug_mode){println("res2="+result)}
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
						if(debug_mode){println("res3="+result)}
						//la règle symétrique
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
						if(debug_mode){println("res4="+result)}						

					}
					return (result)
				}

				/*def next_couple_for_rule_medium_1():(Int,Int)={
					//renvoie le premier couple de cases sur lequel on puisse appliquer rule_medium_1. Renvoie (-1,-1) si un tel couple n'existe pas
					//cherche un couple (n1,n2) tel que les deux soient dans la knowledge_frontier, que n1<n2, que n2 soit "à portée" de n1 (pour qu'ils puissent avoir des voisins communs)
					//et que la règle medium 1 n'ait pas déjà été appliquée sur ce couple
					var v_n2 = -1
					knowledge_frontier.find(n1 => {
						knowledge_frontier.find(n2 => ((n2>n1)&&(n2<=n1 + 2*nb_of_cols + 2)&&!rule_medium_1_application_board(n1)(n2))) match{
						case Some(n2) => {v_n2=n2;true}
						case None => (false)
						}
					}) match{
						case Some(n1) =>(n1,n2)
						case None => (-1,-1)
					}
				}*/

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
					discovered(n_discovered_square)
					neighbour(n_discovered_square).foreach(n => {
						if(game_board(n)!="b"){
							//on a découvert cette case
							rule_easy_1_application_board(n)=false
							rule_easy_2_application_board(n)=false
							/*rule_medium_1_application_board(n)=Array.fill(nb_of_cols*nb_of_rows){false}
							for( i <- 0 to n-1) {
								rule_medium_1_application_board(i)(n)=false
							}*/
						}
					})
					if(game_board(n_discovered_square)=="b"){
						nb_of_found_bombs+=1
						if(nb_of_found_bombs==nb_of_bombs){is_game_solved=true}
					}
				}

				def add_premise(n:Int)={
					premise_board(n)=true
					set_premise_border(n)
				}

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
					//on applique medium_rule_1 sur les couples (n1,n2) tel que les deux soient dans la knowledge_frontier, que n1<n2, que n2 soit "à portée" de n1 
					//(pour qu'ils puissent avoir des voisins communs)
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
				progression=true
				difficulty match{
					case 0 => 	easy_rules_loop()
					case 1 => 	while(!is_game_solved&&progression){
														progression=false
														easy_rules_loop()
														medium_rules_1_everywhere()
								}
					case _ => println("difficulty=" + difficulty.toString)
				}


				def debug_stop(message:String):Unit ={
					if(debug_mode){
						println(message + "  (Entrer pour continuer/Ctrl-C pour quitter)")
						val line = Console.readLine
						if(line=="q"){/*throw new Interruption_in_Debug_Mode("Interruption_in_Debug_Mode")*/ System.exit(0)}
						if(line=="kf"){
							println("x?")
							val x = Console.readLine.toInt
							println("y?")
							val y = Console.readLine.toInt
							println(knowledge_frontier(x + y*nb_of_cols))
							debug_stop("Autre chose ?")}
					}
				}
				
				if(true/*debug_mode*/){
					if(is_game_solved){
						println("Difficulté: " + difficulty)
						debug_stop("Résolution réussie")
					}
					else {
						println("Difficulté: " + difficulty)
						debug_stop("Résolution échouée")
					}
				}
				return(is_game_solved)
			}

			var nb_of_game_creation_tries = 0

			//Choisissez l'un des deux modes suivant: 
			//##BOUNDED MODE##
			//val nb_of_game_creation_tries_limit = 20
			//##ENDLESS MODE##
			def nb_of_game_creation_tries_limit = nb_of_game_creation_tries + 1

			var game_solved = false
			if(debug_mode()){println("Difficulté: " + difficulty)}
			while((nb_of_game_creation_tries < nb_of_game_creation_tries_limit) && !game_solved){
				println("Boucle Création-Résolution, essai "+(nb_of_game_creation_tries + 1))
				generate_a_game_board()
				if(debug_mode){
					game_frame_content.grid.get_contents().foreach(label => label.debug_hide())
					apply_game_board()
				}
				game_solved = solve_game_board()

				nb_of_game_creation_tries += 1

				//if(debug_mode){game_frame_content.grid.access_n(random_gen.nextInt(nb_of_cols*nb_of_rows)).background = DGE.dark_golden_rod1}
				//if(debug_mode){game_frame_content.grid.access_n(random_gen.nextInt(nb_of_cols*nb_of_rows)).reveal()}
				//if(debug_mode){game_frame_content.grid.access_n(random_gen.nextInt(nb_of_cols*nb_of_rows)).hide()}
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



		/*
		neighbour(n_origin_label).foreach(n => grid.access_n(n).value = "#")
		while (bombs_left > 0) {
			var random = random_gen.nextInt(nb_of_rows * nb_of_cols)

			if (grid.access_n(random).value == "?") {
				grid.access_n(random).value = "b"

				bombs_left -= 1
			}
		}
		val grid_label_list = grid.get_contents
		grid_label_list.foreach(label => 
			if (label.value != "b"){
				var new_value = 0
				neighbour(label.numero).foreach(number => 
					if (grid.access_n(number).value == "b") {new_value += 1}
				)
				label.value = new_value.toString
			}
		)

		*/

	}
	//Un label qui se découvre avec une valeur égale à 0 (ie aucun de ses voisins ne contient de bombes) appelle cette fonction pour que ses voisins se découvrent
	def spread(numero : Int) = {
		val grid_content = game_frame_content.grid.get_contents
		var voisins_list = neighbour(numero)
		voisins_list.foreach(numero => grid_content(numero).discover())
		
	}
}

/*
object Main {
	def main(args: Array[String]) {
		val ui = new UI(Demineur)
		ui.visible = true
	}
}*/

}	//accolade fermante du package Demineur
}	//accolade fermante du package Games