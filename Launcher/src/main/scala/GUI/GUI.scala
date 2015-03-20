import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat
import java.awt.event.{ActionEvent, ActionListener}
import java.awt.event._
import scala.swing.ComboBox
import javax.swing.{ImageIcon, Icon}

//import scalafx._

//import javafx.scene.media.Media
//import javafx.scene.media.MediaPlayer

//import javax.sound._
//import java.io._

package GUI{

object GUI_Mood{	//définit les couleurs de l'interface graphique
	val f_colour = GUI_GE.dark_golden_rod1	//couleur des textes ("foreground")
	val b_colour = GUI_GE.maroon4		//couleur des arrières-plans ("background")
}


//Chaque jeu doit l'instancier avec deux tableaux de valeurs correspondant aux valeurs de paramètres de jeu numériques et textuels 
//pour définir les modes de difficulté qu'il souhaite proposer et les regrouper dans la variable game_game_mode_list sous la forme d'une IndexedSeq
//Le paramètre name permet au jeu lorsqu'il définit ses modes de difficulté de donner un nom personnalisé au mode de difficulté
case class Game_Mode(numeric_game_parameters_values_list: IndexedSeq[Int], string_game_parameters_values_list: IndexedSeq[String], name:String="") {
	def get_name (game: Game) :String = {	//L'argument game sert à Cannonical_Game_Mode_Namer pour récupérer les noms des paramètres
		if (name==""){
			Canonical_Game_Mode_Namer.name(game:Game,numeric_game_parameters_values_list: IndexedSeq[Int], string_game_parameters_values_list: IndexedSeq[String])
		}
		else {
			name
		}
	}
	def set_game_parameters (game: Game) ={
		if (numeric_game_parameters_values_list.length == game.numeric_game_parameters_def_list.length) {
			for (i <- 0 until numeric_game_parameters_values_list.length) {
				//Modifie le champ correspondant à la valeur du paramètre numérique i du jeu, en lui assignant la ième valeur 
				//de la liste de valeurs pour les paramètres numérique du jeu du mode de difficulté
				Game_Parameters_Value_Setters.numeric_game_parameter_value_setter(i, numeric_game_parameters_values_list(i), game)
			}
		}
		else {println("Anormal: le nombre de valeurs pour les paramètres numériques du jeu déclarées dans un certain mode de difficulté n'est pas égal au nombre de paramètres numériques de ce jeu")}
		if (string_game_parameters_values_list.length == game.string_game_parameters_def_list.length) {
			for (i <- 0 until string_game_parameters_values_list.length) {
				//Modifie le champ correspondant à la valeur du paramètre textuel i du jeu, en lui assignant la ième valeur 
				//de la liste de valeurs pour les paramètres textuels du jeu du mode de difficulté
				Game_Parameters_Value_Setters.string_game_parameter_value_setter(i, string_game_parameters_values_list(i), game)
			}
		}
		else {println("Anormal: le nombre de valeurs pour les paramètres textuels du jeu déclarées dans un certain mode de difficulté n'est pas égal au nombre de paramètres textuels de ce jeu")}

	}
}
object Canonical_Game_Mode_Namer{	//Sert à la méthode get_name de Game_Mode
	def name(game:Game, numeric_game_parameters_values_list: IndexedSeq[Int], string_game_parameters_values_list: IndexedSeq[String]) :String={
		var game_mode_name = numeric_game_parameters_values_list(0).toString + "x" + numeric_game_parameters_values_list(1).toString
		// Ce gros bloc sert à remplir la variable game_mode_name avec le nom du mode de difficulté, obtenu en mettant bout à bout
		// les différents paramètres de jeu qui constitue le mode de difficulté
		if (numeric_game_parameters_values_list.length > 2) {
			for (i <- 2 until numeric_game_parameters_values_list.length) {
				game_mode_name += ", " + numeric_game_parameters_values_list(i).toString + " " + game.numeric_game_parameters_def_list(i)._1
				//Rajoute ", <nom_du_paramètre_numérique> <valeur_du_paramètre_numérique>" au nom du mode de difficulté
			}
		}
		if (string_game_parameters_values_list.length > 0) {
			for (i <- 0 until string_game_parameters_values_list.length) {
				game_mode_name += ", " + string_game_parameters_values_list(i)
				//Rajoute ", <valeur_du_paramètre_textuel>" au nom du mode de difficulté
			}
		}
		game_mode_name
	}
}

object Game_Parameters_Value_Setters {
	//Cette méthode sert à modifier le champ correspondant à la valeur du paramètre numérique numéro no_parameter du jeu game, en lui assignant la valeur new_value
	def numeric_game_parameter_value_setter (no_parameter: Int, new_value: Int, game: Game) = {
		val old_numeric_parameter_def = game.numeric_game_parameters_def_list(no_parameter)
		var new_numeric_parameter_def = ("", 0, 0, 0)
		old_numeric_parameter_def match {
			case (parameter_name, parameter_value, inf_bound, sup_bound) =>
				new_numeric_parameter_def = (parameter_name, new_value, inf_bound, sup_bound)
		}
		game.numeric_game_parameters_def_list = game.numeric_game_parameters_def_list.updated(no_parameter, new_numeric_parameter_def)
		 	//la méthode updated renvoie une autre liste identique à la première dans 
			//laquelle le ième terme à été remplacé par le terme donné en deuxième 
			//argument. On en a besoin ici car les IndexedSeq de types combiné 
			//(ex: [Int,Int,IndexedSeq[String]]) sont immutables et donc non 
			//modifiables de façon classique
	}
	//Cette méthode sert à modifier le champ correspondant à la valeur du paramètre textuel numéro no_parameter du jeu game, en lui assignant la valeur new_value
	def string_game_parameter_value_setter (no_parameter: Int, new_value: String, game: Game) = {
		val old_string_parameter_def = game.string_game_parameters_def_list(no_parameter)
		var new_string_parameter_def = ("", "", IndexedSeq(""))
		old_string_parameter_def match{
			case (parameter_name, parameter_value, parameter_possible_values) =>
				new_string_parameter_def = (parameter_name, new_value, parameter_possible_values)
		}
		game.string_game_parameters_def_list = game.string_game_parameters_def_list.updated(no_parameter,new_string_parameter_def)
			//la méthode updated renvoie une autre liste identique à la première dans 
			//laquelle le ième terme à été remplacé par le terme donné en deuxième 
			//argument. On en a besoin ici car les IndexedSeq de types combiné 
			//(ex: [Int,Int,IndexedSeq[String]]) sont immutables et donc non 
			//modifiables de façon classique
	}
}

//Signature d'un jeu
abstract class Game{
	val title: String
	val square_size_x: Int 	//largeur des cases
	val square_size_y: Int	//hauteur des cases

	var game_beginning_time: Date //date de début de la partie pour le chronomètre

	var playing = false //Vaut true si une partie est en cours (ie : le chrono tourne) et faux sinon
	var in_game = false	//Vaut true si une partie a jamais été lancée, faux sinon (ie on est à l'écran "Welcome ;)" )

	var numeric_game_parameters_def_list: IndexedSeq[(String, Int, Int, Int)]		//liste des paramètres numériques du jeu sous la forme de tuples 
																				//(nom, valeur, borne_inf_pour_mode_custom, borne_sup_pour_mode_custom) et dont les deux 
																				//premiers doivent etre "Width" et "Height" (resp le nb de colonnes de la grille et 
																				//le nb de lignes de la grille)
	var string_game_parameters_def_list: IndexedSeq[(String, String, IndexedSeq[String])]	//liste des paramètres chaines de caractères du jeu sous la forme de tuples 
																							//(nom, valeur, IndexedSeq_des_valeurs_possibles_de_ce_paramètre)

	type Game_Label_Class <: Grid_Label	//Les labels avec lesquels sera remplis la grille (par la classe Grid)
	def glb_factory () : Game_Label_Class //Une usine à labels de la classe Game_Label_Class
	type Game_Border_Label_Class <: Label 	//Les labels avec lesquels seront remplis les bords de la grille
	def gblb_factory () : Game_Border_Label_Class	//une usine à labels de la classe Game_Border_Label_Class
	def about_frame_factory (): Frame 		//une fonction qui fournit la fenetre "About" du jeu
	def help_frame_factory (): Frame 		//une fonction qui fournit la fenetre "Help" du jeu

	var random_gen = new scala.util.Random()	//Le générateur aléatoire utilisé par le jeu
	var game_frame_content : Game_Frame_Content[Game_Label_Class, Game_Border_Label_Class] = null 	//Variable stockant le contenu graphique de la fenetre de jeu lors d'une partie

	val game_game_mode_list : IndexedSeq[Game_Mode] 	//Liste des modes de difficulté que le jeu veut proposer
	def custom_game_parameters_conditions (form_nb_fields_result: IndexedSeq[Int]): String	//Une fonction qui, aux résultat des champs numériques d'un formulaire
																							// de partie custom, vérifie des conditions propres au jeu 
																							//(si ces paramètres permettent de jouer au jeu ou non) et 
																							//renvoie "OK" si les valeurs reçus vérifient ces conditions et une 
																							//string contenant le message d'erreur sinon
	def game_starter (): Unit 	// game_starter ne contient que les choses à faire avant de lancer une partie qui sont spécifiques au jeu, le reste 
								//est fait dans generic_game_starter
	def game_action_restart (): Unit 	//game_action_restart ne contient que les choses à faire avant de relancer une partie qui sont spécifique au jeu, le reste 
										//est fait dans generic_action_restart
	def launch_game_timer() = {
		if (!playing && !end_lock) {
			playing = true
			game_beginning_time = new Date()
			game_frame_content.timer_label.restart(game_beginning_time)			
		}

	}

	var end_lock = false

	def win() = {
		end_lock = true
		val outcome_label = game_frame_content.outcome_label
		//val timer_label = game_frame_content.timer_label
		val grid_content = game_frame_content.grid.get_contents
		outcome_label.text = "GAGNÉ !"
		outcome_label.background = new Color(0,139,0)
		//timer_label.stop()
		game_frame_content.timer_label.stop()
		grid_content.foreach(label => label.deafTo(label.mouse.moves, label.mouse.clicks))
		playing = false
	}
	def lose() = {
		end_lock = true
		val outcome_label = game_frame_content.outcome_label
		//val timer_label = game_frame_content.timer_label
		val grid_content = game_frame_content.grid.get_contents
		outcome_label.text = "PERDU !"
		outcome_label.background = new Color(255,0,0)
		//timer_label.stop()
		game_frame_content.timer_label.stop()
		grid_content.foreach(label => label.deafTo(label.mouse.moves, label.mouse.clicks))	
		playing = false	
	}
}

//Crée le contenu de la fenetre de jeu (labels du bandeau inférieur et grille)
class Game_Frame_Content[Game_Label_Class <: Grid_Label, Game_Border_Label_Class <: Label] (game: Game) extends GridBagPanel {
    def constraints(x: Int, y: Int, 
		    gridwidth: Int = 1, gridheight: Int = 1,
		    weightx: Double = 0.0, weighty: Double = 0.0,
		    fill: GridBagPanel.Fill.Value = GridBagPanel.Fill.None) 
    : Constraints = {
      val c = new Constraints
      c.gridx = x
      c.gridy = y
      c.gridwidth = gridwidth
      c.gridheight = gridheight
      c.weightx = weightx
      c.weighty = weighty
      c.fill = fill
      c
    }

	val label_1 = new Label(){background = GUI_Mood.b_colour; foreground = GUI_Mood.f_colour; opaque = true}
	val label_2 = new Label(){background = GUI_Mood.b_colour; foreground = GUI_Mood.f_colour; opaque = true}
	val outcome_label = new Label(){background = GUI_Mood.b_colour; foreground = GUI_Mood.f_colour}
		outcome_label.opaque = true
	val timer_label = new Timer_Label(game.game_beginning_time){background = GUI_Mood.b_colour; foreground = GUI_Mood.f_colour; opaque = true}
	var grid = new Grid[Game_Label_Class](game)
	val bottom_panel = new FlowPanel() {
		background = GUI_Mood.b_colour
		foreground = GUI_Mood.f_colour
		contents += label_1
		contents += label_2
		contents += outcome_label
		contents += timer_label
	}
	val gfc_game = game
	/*var top_grid = new Border_Grid[gfc_game.Game_Border_Label_Class] (nb_of_cols, gfc_game.gblb_factory)
	var right_grid = new Border_Grid[gfc_game.Game_Border_Label_Class] (nb_of_rows, gfc_game.gblb_factory)
	var bottom_grid = new Border_Grid[gfc_game.Game_Border_Label_Class] (nb_of_cols, gfc_game.gblb_factory)
	var left_grid = new Border_Grid[gfc_game.Game_Border_Label_Class] (nb_of_rows, gfc_game.gblb_factory)
	*/
	val vertical = Border_Grid_Vertical()
	val horizontal = Border_Grid_Horizontal()

	def label_factory () ={new Label }
	/*var top_border_grid   = new Border_Grid[Game_Border_Label_Class] (nb_of_cols, game.gblb_factory, vertical, game.square_size_x, game.square_size_y)
	var right_border_grid  = new Border_Grid[Game_Border_Label_Class] (nb_of_rows, game.gblb_factory, vertical, game.square_size_x, game.square_size_y)
	var bottom_border_grid  = new Border_Grid[Game_Border_Label_Class] (nb_of_cols, game.gblb_factory, vertical, game.square_size_x, game.square_size_y)
	var left_border_grid  = new Border_Grid[Game_Border_Label_Class] (nb_of_rows, game.gblb_factory, vertical, game.square_size_x, game.square_size_y)
	*/
	var top_border_grid : Border_Grid[Game_Border_Label_Class] = null
	var right_border_grid : Border_Grid[Game_Border_Label_Class] = null
	var bottom_border_grid : Border_Grid[Game_Border_Label_Class] = null
	var left_border_grid : Border_Grid[Game_Border_Label_Class] = null
	var top_left_label = new Label
	var top_right_label = new Label
	var bottom_left_label= new Label
	var bottom_right_label = new Label

	add(bottom_panel, 
		constraints(1, 3, fill = GridBagPanel.Fill.Horizontal, weightx = 1))
	add(grid,
    	constraints(1, 1, fill = GridBagPanel.Fill.Both, weightx = 1, weighty = 1))

	def set_top_border_grid () ={
		top_border_grid = new Border_Grid[Game_Border_Label_Class] (nb_of_cols, game.gblb_factory, horizontal, game.square_size_x, game.square_size_y)
		add(top_border_grid,
			constraints(1, 0, fill = GridBagPanel.Fill.Both, weightx = 1, weighty = 1.0/nb_of_rows))
	}
	def set_right_border_grid () ={
		right_border_grid = new Border_Grid[Game_Border_Label_Class] (nb_of_rows, game.gblb_factory, vertical, game.square_size_x, game.square_size_y)
		add(right_border_grid,
			constraints(2, 1, fill = GridBagPanel.Fill.Both, weightx = 1.0/nb_of_cols, weighty = 1))
		revalidate()
	}
	def set_bottom_border_grid () ={
		bottom_border_grid = new Border_Grid[Game_Border_Label_Class] (nb_of_cols, game.gblb_factory, horizontal, game.square_size_x, game.square_size_y)
		add(bottom_border_grid,
			constraints(1, 2, fill = GridBagPanel.Fill.Both, weightx = 1, weighty = 1.0/nb_of_rows))
	}
	def set_left_border_grid () ={
		left_border_grid = new Border_Grid[Game_Border_Label_Class] (nb_of_rows, game.gblb_factory, vertical, game.square_size_x, game.square_size_y)
		add(left_border_grid,
			constraints(0, 1, fill = GridBagPanel.Fill.Both, weightx = 1.0/nb_of_cols, weighty = 1))
	}
	def set_top_left_border_label () ={
		top_left_label = game.gblb_factory()
		add(top_left_label,
			constraints(0, 0, fill = GridBagPanel.Fill.Both, weightx = 1.0/nb_of_cols, weighty = 1.0/nb_of_rows))
	}
	def set_top_right_border_label () ={
		top_right_label = game.gblb_factory()
		add(top_right_label,
			constraints(2, 0, fill = GridBagPanel.Fill.Both, weightx = 1.0/nb_of_cols, weighty = 1.0/nb_of_rows))
	}
	def set_bottom_left_border_label () ={
		bottom_left_label = game.gblb_factory()
		add(bottom_left_label,
			constraints(0, 2, fill = GridBagPanel.Fill.Both, weightx = 1.0/nb_of_cols, weighty = 1.0/nb_of_rows))
	}
	def set_bottom_right_border_label () ={
		bottom_right_label = game.gblb_factory()
		add(bottom_right_label,
			constraints(2, 2, fill = GridBagPanel.Fill.Both, weightx = 1.0/nb_of_cols, weighty = 1.0/nb_of_rows))
	}
	
	val nb_of_cols = game.numeric_game_parameters_def_list(0)._2
	val nb_of_rows = game.numeric_game_parameters_def_list(1)._2
	background = GUI_Mood.b_colour
}

//A class used in the method add_border_int_label in class Game_Frame_Content for the parameter location.
abstract class GFC_Location{}
case class GFC_Top extends GFC_Location
case class GFC_Bottom extends GFC_Location
case class GFC_Right extends GFC_Location
case class GFC_Left extends GFC_Location

/*Inutile Ici, conservé pour références futures
//Une exception lancée par la fonction game_custom_mode d'un jeu lorsque les paramètres numériques renvoyés par le formulaire ne permettent pas de créer une partie du jeu
case class Custom_Mode_Exception(value: String) extends Throwable{}
*/

//Cet objet permet au jeu d'accéder à son ui. Utilisation déconseillée mais on a parfois pas le choix
object UI_Link {
	var actual_ui: Frame = null
}


//UI est la fenetre principale des jeux
class UI (game: Game) extends Frame {

	/*music = "Enticement.mp3"
	media_music = new Media(music)
	media_player = new MediaPlayer(media_music)
	media_player.play()*/

	/*var audioInputStream : javax.sound.sampled.AudioInputStream = null;
	var musicfile = new File("src/main/ressources/AngelWar/08McCann-VigiloConfido.wav")
	println(musicfile)
 	try{
 			//obtention d'un flux audio à partir d'un fichier (objet File)
      audioInputStream = javax.sound.sampled.AudioSystem.getAudioInputStream(musicfile);

    } catch {
    	case e: javax.sound.sampled.UnsupportedAudioFileException => {e.printStackTrace()}
    	case e: IOException => {e.printStackTrace()}
    }
	//Il est nécessaire de connaître le format audio du fichier
	// d'entrée
	// pour permettre à java de créer l'objet DataLine adéquat
	val audioFormat : javax.sound.sampled.AudioFormat= audioInputStream.getFormat();
	 // En plus du format du flux audio d'entrée il est nécessaire de
	 // spécifier le type de DataLine qu'on veut
	 // ici le DataLine qu'on souhaite est un SourceDataLine qui permet
	 // la
	 // lecture (targetDataLine permet l'enregistrement).
	var info : javax.sound.sampled.DataLine.Info = new javax.sound.sampled.DataLine.Info(javax.sound.sampled.SourceDataLine.class, audioFormat);
	 // On récupère le DataLine adéquat et on l'ouvre
	var line:  javax.sound.sampled.SourceDataLine ;
	 try {
	 line = javax.sound.sampled.AudioSystem.getLine(info).asInstanceOf[javax.sound.sampled.SourceDataLine];
	           
	 } catch  {
	 	case e: javax.sound.sampled.LineUnavailableException => e.printStackTrace();
	 }*/



	iconImage = toolkit.getImage(getClass.getResource("/my_purple_dice_20.png"))
	val timer_listener = new ActionListener{
		def actionPerformed(e: ActionEvent) {
			thisui.minimumSize = thisui.preferredSize
		}
	}
	val timer = new javax.swing.Timer(1000, timer_listener)
	timer.start()




	val thisui = this
	title = game.title
	//resizable = false
	contents = new Label(){
		font = new Font("Arial", 1, 20)
		//text = "Welcome to " + game.title + " ! ;)"
		text = game.title
		val lighter_maroon4 = new Color(179, 70, 138)
		val darker_maroon4 = new Color(109, 0, 68)
		val width = 300
		val height = 300
		this.peer.setOpaque(false);				//Cetteligne dit à la méthode paint de ne pas changer le background
		override def paint(g: Graphics2D)={
			val background_gradientpaint = new java.awt.GradientPaint(0, 0, lighter_maroon4, 0, thisui.size.height, darker_maroon4)
			g.setPaint(background_gradientpaint)
			g.fillRect(0, 0, thisui.size.width, thisui.size.height)
			super.paint(g)
		}
		preferredSize = new Dimension(width,height)
		//background = GUI_Mood.b_colour
		//opaque = true
		foreground = GUI_Mood.f_colour
	}
	val Game_Starter = new Generic_Game_Starter(game,thisui)
	val Action_Restart = new Generic_Action_Restart(game)

	def action_generic_random_seed() {
		if (game.in_game){
			var random_seed_form = new Form(
				"Graine Aléatoire",
				IndexedSeq(("Random Seed",0,0)),
				null,
				any => "OK")
			val asked_random_seed = random_seed_form.nb_fields_results(0)
			game.random_gen = new scala.util.Random(asked_random_seed)

			//Action_Restart.action_restart()
			Game_Starter.generic_game_starter()
		}	
	}

	def action_generic_custom_mode()  {
		val nb_fields_def_list = game.numeric_game_parameters_def_list map (numeric_game_parameter_def => 
			numeric_game_parameter_def match {
				case (parameter_name, parameter_value, parameter_inf_bound, parameter_sup_bound) =>
					(parameter_name, parameter_inf_bound, parameter_sup_bound)
			}
		)
		val comboboxes_def_list = game.string_game_parameters_def_list map (string_game_parameter =>
			string_game_parameter match {
				case (parameter_name, parameter_value, parameter_possible_values) => 
					(parameter_name, parameter_possible_values)
			}
		)
			val custom_game_form = new Form(
				"Partie Personnalisée",
				nb_fields_def_list,
				comboboxes_def_list,
				game.custom_game_parameters_conditions)
			val form_nb_fields_results = custom_game_form.nb_fields_results		//Les résultats numériques du formulaire
			val form_comboboxes_results = custom_game_form.comboboxes_results	//Les résultats textuels du formulaire	 
			var custom_game_mode = Game_Mode(form_nb_fields_results, form_comboboxes_results)
			if (custom_game_form.form_accepted) {	// Vérifie que le formulaire soit accepté 
													//(en pratique, ça attend que le joueur ait cliqué
													// sur le bouton fini),
				custom_game_mode.set_game_parameters(game)
				Game_Starter.generic_game_starter()
			}
			else {println("le formulaire a été fermé")}
	}
	//"MIM" signifie "MenuItemMaker"
	class Playmenu_MIM(game_mode: Game_Mode) extends MenuItem(""){ //Fabrique un élément du menu Play à partir de l'un des modes de difficulté spécifiés par le jeu
		def menuitem_action () = {
			game_mode.set_game_parameters(game)
			Game_Starter.generic_game_starter()
		}
		action = Action(game_mode.get_name(game))(menuitem_action)
	}

	//Définition préventive de ces deux MenuItem pour que Generic_Game_Starter les dégrisent (enabled = true) lorsqu'une partie est lancé
	val restart_menuitem = new MenuItem(""){action = Action("Recommencer")(Action_Restart.action_restart())
											enabled = false
											background = GUI_Mood.b_colour
											foreground = GUI_Mood.f_colour
											}
	val randomseed_menuitem = new MenuItem("")	{action = Action("Graine Aléatoire...")(action_generic_random_seed())
												enabled = false
												background = GUI_Mood.b_colour
												foreground = GUI_Mood.f_colour
											}
	menuBar = new MenuBar {
		background = GUI_Mood.b_colour
		contents += new Menu("Jouer") {
			foreground = GUI_Mood.f_colour
			game.game_game_mode_list.foreach(game_mode =>
				contents += new Playmenu_MIM(game_mode){
					background = GUI_Mood.b_colour
					foreground = GUI_Mood.f_colour
				}
			)
			contents += new MenuItem(""){background = GUI_Mood.b_colour}
			contents += new MenuItem(""){action = Action("Partie Personalisée...")(action_generic_custom_mode()); background = GUI_Mood.b_colour; foreground= GUI_Mood.f_colour}
		}
		contents += new Menu("Partie") {
			background = GUI_Mood.b_colour
			foreground = GUI_Mood.f_colour
			contents += restart_menuitem
			contents += randomseed_menuitem
			contents += new MenuItem(""){background = GUI_Mood.b_colour}
			contents += new MenuItem(""){action = Action("Quitter") {System.exit(0)}; background = GUI_Mood.b_colour; foreground = GUI_Mood.f_colour}
		}
		contents += new Menu("Aide") {
			background = GUI_Mood.b_colour
			foreground = GUI_Mood.f_colour
			contents += new MenuItem(""){action = Action("A Propos")(game.about_frame_factory()); background = GUI_Mood.b_colour; foreground = GUI_Mood.f_colour}
			contents += new MenuItem(""){action = Action("Aide du " + game.title)(game.help_frame_factory()); background = GUI_Mood.b_colour; foreground = GUI_Mood.f_colour}
		}
	}

	class Generic_Action_Restart (game: Game) {
		def action_restart() ={
			if (game.in_game) {
				game.playing = false
				game.end_lock = false
				game.game_frame_content.timer_label.reset_text()
				game.game_frame_content.timer_label.restart(new Date())
				game.game_frame_content.timer_label.stop()
				game.game_action_restart()
				val outcome_label = game.game_frame_content.outcome_label
				outcome_label.text = ""


				/*game.game_beginning_time = new Date()
				game.game_frame_content.timer_label.restart(game.game_beginning_time)
				game.game_frame_content.timer_label.stop()*/
				

				//MODIF
				/*val timer_label = game.game_frame_content.timer_label
				timer_label.restart(game.game_beginning_time)*/
			}
		}
	}

	class Generic_Game_Starter (game: Game, ui: Frame) {
		def generic_game_starter (): Unit ={
			game.in_game = true
			game.end_lock = false
			restart_menuitem.enabled = true //Dégrise les menuItem restart et random_seed
			randomseed_menuitem.enabled = true
			game.playing = false
			game.game_beginning_time = new Date()

			game.game_frame_content = new Game_Frame_Content[game.Game_Label_Class,game.Game_Border_Label_Class](game)
			//game.game_frame_content.timer_label.restart(game.game_beginning_time)
			//game.game_frame_content.timer_label.stop()

			//MODIF
			val outcome_label = game.game_frame_content.outcome_label
			outcome_label.text = ""
			
			ui.contents = game.game_frame_content//.final_content
			UI_Link.actual_ui = thisui
			///game_frame_content.timer_label.restart(new Date())
			//game_frame_content.timer_label.stop() //Le jeu doit lancer le timer label quand il veut

			//MODIF
			/*game.game_action_restart()
			game.in_game = false*/
			game.game_starter()



			//game_frame_content.bottom_panel.maximumSize = game_frame_content.bottom_panel.preferredSize
			thisui.minimumSize = thisui.preferredSize
			thisui.size = thisui.minimumSize

		}
	}
	thisui.minimumSize = thisui.preferredSize
	//Inutile mais conservé pour réfèrence future
	/*
	val resize_reactor = new Object with Reactor
	resize_reactor.listenTo(thisui)
	resize_reactor.reactions += {
		case UIElementResized(uielement) => {
			}
		}
	}*/
}

}	//Accolade fermante du package GUI

