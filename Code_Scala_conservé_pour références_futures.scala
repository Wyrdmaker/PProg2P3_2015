//Aliasser des variables:
//#
	def nb_of_bombs = game_parameter_1 //Ces deux fonctions font de nb_of_bombs un alias de la variable game_parameter_1
	def nb_of_bombs_=(newval: Int) { game_parameter_1 = newval }
//#

//Déclarer des exceptions:
//#
case class Custom_Mode_Exception(value: String) extends Throwable{}
//#

###_PEINTURE_###

//Pour peindre une image:
//#
	ImageIcon icon = new ImageIcon("whatever.jpg");
	//Make sure the reference you create is an ImageIcon reference. Then use getImage() to grab the image from the ImageIcon:
	Image img = icon.getImage();
	//Now create a buffered image the same size as the image:
	BufferedImage bi = new BufferedImage(img.getWidth(null), img.getHeight(null), BufferedImage.TYPE_INT_ARGB);
	//Then blit the icon image to the buffered image, and resize it as you do so:
	Graphics g = bi.createGraphics();
	g.drawImage(img, 0, 0, WIDTH, HEIGHT, null);
	//(The code above may be incorrect - check the docs)
	//Now recreate the IconImage with the new buffered image:
	IconImage newIcon = new IconImage(bi);
//#

//Pour peindre des dégradés:
//#
	val foreground_gradientpaint = new java.awt.GradientPaint(0, 0, GUI_GE.silver, 0, 10, darker_silver)
//#

//Pour créer des bordures avancées:
//#
	border = javax.swing.BorderFactory.createEtchedBorder(javax.swing.border.EtchedBorder.RAISED, GUI_GE.dark_golden_rod1, GUI_GE.dark_orchid)
	border = javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED, GUI_GE.dark_golden_rod1, GUI_GE.yellow)
	border = javax.swing.BorderFactory.createCompoundBorder(outside = Swing.LineBorder(GUI_GE.maroon4, 2), inside = Swing.LineBorder(GUI_GE.dark_golden_rod1, 2))
//#

###_PARTIE_LONGUES_###

//Code qui servait à faire dire au personnage qqchose quand la partie devenait longue, retiré car ça se produisait même si on changeait de jeu entre temps
//#
var long_play_reactor = new Object with Reactor{
	var listened_timer : Timer_Label = null
	override def listenTo (ps: Publisher) ={
		super.listenTo(ps)
		listened_timer = ps
	}
	def clear () ={
		deafTo(listened_timer)
	}
}
listenTo(this)
reactions +={
	case e: WindowClosing => {println("wc");long_play_reactor = null}
}
//#

//Code qui servait à faire dire au personnage qqchose quand la partie devenait longue, retiré car ça se produisait même si on changeait de jeu entre temps
//#
if(game.enabled_main_character_speak_on_long_play){
	//val long_play_reactor = new Object with Reactor 	//Cet objet va faire dire quelque chose au main character si la partie devient longue
	//var active = true
	long_play_reactor = new Object with Reactor{
		listenTo(game.game_frame_content.timer_label)
		reactions += {
			case Minute_Tick(minute, timer) => {
				if(minute == game.main_character_acceptable_time){
					Main.main_character.say_smth(game.main_character_text_on_long_play)
					deafTo(timer)
				}
			}
			case Timer_Stop(timer_stopped) => {
				deafTo(timer_stopped)
			}
		}		
	}
}
//#

//Code dans GUI.Game, qui définnissait des paramètres pour que le personnage principal dise qqchose quand la partie devenait longue
//#
var main_character_text_on_long_play: Array[String] = Array("Tu n'est pas très rapide...","Moi, à ta place, j'aurais déjà fini !")
var main_character_acceptable_time: Int = 3	//Le temps au bout duquel le personnage principal dit au joueur qu'il traine (en minutes)
var enabled_main_character_speak_on_long_play: Boolean = true	//Permet au jeu d'empecher le main_character de dire quelque chose quand la partie devient longue
//#

###_DIVERS_###

//Code pour définir un objet qui permettait de faire une action à chaque fois que  l'UIElement était retaillé
//#
val resize_reactor = new Object with Reactor
resize_reactor.listenTo(thisui)
resize_reactor.reactions += {
	case UIElementResized(uielement) => {
		}
	}
}
//#

//La méthode updated:
//#
	//la méthode updated renvoie une autre liste identique à la première dans 
	//laquelle le ième terme à été remplacé par le terme donné en deuxième 
	//argument. On en a besoin ici car les IndexedSeq de types combiné 
	//(ex: [Int,Int,IndexedSeq[String]]) sont immutables et donc non 
	//modifiables de façon classique
//#

//L'ancienne class Number_Form, remplacée par la classe Form, plus générale
//#
//La classe Number Form permet de demander au joueur des renseignements chiffrés
//Les noms des champs doivent etre fournis sous forme de IndexedSeq -> fields_names_list
//Les couples d'Int de fields_bounds_list représentent le min et le max que l'utilisateur peut rentrer dans le formulaire pour chaque champs
//( un couple de la forme (n,n) avec n un entier signifie pas de restriction )
//La valeur par défaut des champs sera la moyenne du min et du max du champs en question
class Number_Form(titre : String, fields_names_list : IndexedSeq[String], fields_bounds_list : IndexedSeq[(Int,Int)]) extends Dialog {
	var result: IndexedSeq[Int] = fields_bounds_list map (couple => couple._1)
	var form_accepted : Boolean = false
	if (fields_names_list.length == fields_bounds_list.length) {
		title = titre
		modal = true
		var number_fields_list = fields_bounds_list map (couple =>
			couple match {
				case (min_value,max_value) => new Number_Field(((max_value + min_value)/2).toString)
			})
		contents = new GridPanel(fields_names_list.length + 1, 2) {
			for (i <- 0 until fields_names_list.length) {
				var bounds_string = "  (" + fields_bounds_list(i)._1 + "/" + fields_bounds_list(i)._2 + ")"
				if (fields_bounds_list(i)._1 == fields_bounds_list(i)._2) { bounds_string = ""}
				contents += new Label(fields_names_list(i) + bounds_string + " : ")
				contents += number_fields_list(i)
			}
			contents += new Label("")
			contents += new Button("") {
				action = Action("Fini")(submit)
			}
		}

		def submit = {
			var nonempty_condition = true
			for (i <- 0 to result.length -1) { // Sert à vérifier que tout les champs du formulaire ont été remplis avant le clic sur le bouton "fini"
				if (number_fields_list(i).text.length <= 0){
					nonempty_condition = false
					number_fields_list(i).text = ((fields_bounds_list(i)._1 + fields_bounds_list(i)._2)/2).toString
				}
			}
			if (nonempty_condition == true) {
				result = number_fields_list map (number_field => number_field.text.toInt)
				var bound_condition = true
				for (i <- 0 to result.length - 1 ) { // Sert à vérifier que les valeurs entrées dans les champs du formulaires sont bien dans les limites définies par fields_bounds_list
													// Si ca n'est pas le cas, réinitialise la valeur du champ incorrect avec la moyenne de ses bornes inf et sup
					if (!((fields_bounds_list(i)._1 <= result(i) && result(i) <= fields_bounds_list(i)._2)
						|| fields_bounds_list(i)._1 == fields_bounds_list(i)._2)) {
						bound_condition = false
						number_fields_list(i).text = ((fields_bounds_list(i)._1 + fields_bounds_list(i)._2)/2).toString
					}
				}
	
				if (bound_condition) {	// Le formulaire a été correctement remplis, on "supprime la fenetre du formulaire et 
										// la fonction qui a appelée le formulaire peut en récupérer les résultats dans l'IndexedSeq "result"
					form_accepted = true
					visible = false
				}
				else {
					println("Les réponses aux formulaires ne sont pas dans les bornes définies")
				
				}
			}
			else {
				println("Certains des champs du formulaire sont vides")
			}
			

		}

		visible = true
	}
	else {
		println("Anormal: La classe Number_Form a été instanciée avec deux listes de tailles différentes")
		println(fields_names_list.length)
		println(fields_bounds_list.length)
	}
}
//#

###_MUSIQUE_###

//Librairies utilisées pour essayer d'avoir de la musique
//#
	import java.net.URL
	import javax.sound.sampled._
	import scalafx._
	import scalafx.scene.media.Media
	import scalafx.scene.media.MediaPlayer
	import javafx.scene.media.Media
	import javafx.scene.media.MediaPlayer

	import javax.sound._
	import java.io._
//#

//Code de AngelWar_Help_Frame pour essayer de mettre de la musique (ne marche pas)
//#
	val url = new URL("http://mywebpages.comcast.net/jdeshon2/wave_files/jad0001a.wav")
	val audioIn = AudioSystem.getAudioInputStream(url)
	val clip = AudioSystem.getClip
	clip.open(audioIn)
	clip.start

	//Dans le texte en html:
		"<audio controls> " +
		"<source src=\"src/main/ressources/AngelWar/Enticement.mp3\" type=\"audio/mpeg\"> " +
		"Your browser does not support the audio element. " +
		"</audio> </html>"
//#

//Code dans GUI.UI (au début) pour essayer de mettre de la musique (ne marche pas (n'arrive pas à transformer le fichier musical en un flux audio je crois))
//#
	val music = new Media("src/main/resources/War_of_Angels.mp3")

	music = "Enticement.mp3"
	media_music = new Media(music)
	media_player = new MediaPlayer(media_music)
	media_player.play()*/

	var audioInputStream : javax.sound.sampled.AudioInputStream = null;
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
	 }
//#