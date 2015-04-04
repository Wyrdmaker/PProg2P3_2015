import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat
import java.awt.event.{ActionEvent, ActionListener}

package GUI{

class Number_Field(init_string : String) extends TextField(init_string) {
	listenTo(keys)
	reactions += {
		case e : KeyTyped =>
			if (!e.char.isDigit)
				e.consume
	}
}

//Une sous classe de Number_Field équippée d'une sécurité. Si le champs est laissé vide ou si la valeur entrée n'est pas entre les 
//deux bornes, la valeur du champs sera remises à default_value.
//Comme d'habitude, si inf_bound = sup_bound, il n'y a pas de restrictions
class Secured_Number_Field(default_value: Int, inf_bound: Int, sup_bound: Int) extends Number_Field(default_value.toString){
	background = GUI_Mood.b_colour
	foreground = GUI_Mood.f_colour
	opaque = true

	listenTo(this)
	reactions += {
		case e: EditDone =>
			if (this.text.length == 0 || this.text.length > 9)	//Taille du nombre limitée à 9 chiffres car sinon, .toInt renvoie l'exception: java.lang.NumberFormatException
				this.text = default_value.toString
			else if (!(inf_bound <= this.text.toInt && this.text.toInt <= sup_bound) && !(inf_bound == sup_bound))
					this.text = default_value.toString

	}
}
//La classe Form est une version améliorée de la classe Number_Form dans l'idée (mais ces deux classes sont indépendantes et s'utilisent différemment)
//Form permet de créer un formulaire avec des champs numériques avec le meme système de bornes que Number_Form (bien que la façon de définir un champs numérique ait changé), mais le
//formulaire peut également contenir des listes déroulantes pout demander des paramètres textuels (ce sont les "comboboxes")
//Le paramètre "special_condition" est une fonction prenant en entrée une IndexedSeq des valeurs des champs numériques entrées par l'utilisateur et qui renvoie "OK" si les résultats sont acceptables ou une string contenant le message d'erreur sinon
//les élements de nb_fields_def_list sont de la forme: (nom_du_champ_numérique, bornes_inf_du_résultat_attendu, borne_sup_du_résultat_attendu) (si borne_inf et borne_sup sont égales, il n'y a pas de contraintes)
//les éléments de comboxes_def_list sont de la forme: (nom_de_la_combobox, IndexedSeq_des_chaines_de_charactères_de_la_combobox)
//les résulats des champs numériques sont dans l'IndexedSeq nb_fields_results. Les résultats des champs textuels (comboboxes) sont dans l'IndexedSeq comboboxes_results
class Form(titre : String, nb_fields_def_list: IndexedSeq[(String,Int,Int)], comboboxes_def_list: IndexedSeq[(String, IndexedSeq[String])], special_condition: IndexedSeq[Int] => String) extends Dialog(){
	val this_form = this
	var nb_fields_results: IndexedSeq[Int] = null
	var comboboxes_results: IndexedSeq[String] = null
	var nb_fields_list: IndexedSeq[Number_Field] = null
	var comboboxes_list = IndexedSeq(new ComboBox(Seq("bidon1", "bidon2")))
	var nb_fields_panel = new GridPanel(1,1) //Valeur bidon
	var comboboxes_panel = new GridPanel(1,1) //Valeur bidon
	val no_nb_fields = (nb_fields_def_list == null) //Permet de savoir si le formulaire contient au moins un champs numérique ou non
	val no_comboboxes = (comboboxes_def_list == null) //Permet de savoir si le formulaire contient au moins une combobox ou non
	var form_accepted : Boolean = false
	title = titre
	modal = true
	//Création des Number Fields
	if (!no_nb_fields) {
		nb_fields_results = nb_fields_def_list map (nb_field_def => nb_field_def._2)
		nb_fields_list = nb_fields_def_list map (nb_field_def =>
			nb_field_def match {
				case (nb_field_name, inf_bound, sup_bound) =>
				new Secured_Number_Field(((inf_bound + sup_bound)/2), inf_bound, sup_bound)//{listenTo(this_form)}
			}
		)
		nb_fields_panel = new GridPanel(nb_fields_def_list.length + 1, 2){

			background = GUI_Mood.b_colour
			foreground = GUI_Mood.f_colour

			for (i <- 0 until nb_fields_def_list.length) {
				nb_fields_def_list(i) match {
					case (field_name, inf_bound, sup_bound) =>
						var bounds_string = " (" + inf_bound + "/" + sup_bound + ")"
						if (inf_bound == sup_bound) { bounds_string = ""}
						contents += new Label(field_name + bounds_string + " : "){background = GUI_Mood.b_colour; foreground = GUI_Mood.f_colour; opaque = true}
						contents += nb_fields_list(i)						
				}

			}
			contents += new Label("")
		}
	}
	//Création des Comboboxes
	if (!no_comboboxes){
		comboboxes_results = comboboxes_def_list map (combobox => "")
	
		comboboxes_list = comboboxes_def_list map (combobox_def => 
			combobox_def match {
				case	(combobox_name, combobox_item_seq) =>
					new ComboBox(combobox_item_seq){background = GUI_Mood.b_colour; foreground = GUI_Mood.f_colour; opaque = true}
			}
		)		
		comboboxes_panel = new GridPanel(comboboxes_def_list.length + 1, 2){

			background = GUI_Mood.b_colour
			foreground = GUI_Mood.f_colour

			for (i <- 0 until comboboxes_def_list.length) {
				contents += new Label(comboboxes_def_list(i)._1){background = GUI_Mood.b_colour; foreground = GUI_Mood.f_colour; opaque = true}
				contents += comboboxes_list(i)
			}
			contents += new Label(""){background = GUI_Mood.b_colour; foreground = GUI_Mood.f_colour; opaque = true}
		}
	}
	//Contenu final du formulaire
	val form_panel = new BoxPanel(Orientation.Vertical){

		background = GUI_Mood.b_colour
		foreground = GUI_Mood.f_colour

		contents += nb_fields_panel
		contents += comboboxes_panel
		contents += new Button("") {
			background = GUI_Mood.b_colour
			foreground = GUI_Mood.f_colour

			action = Action("Fini")(submit)

		}
		def submit = {
			nb_fields_results = nb_fields_list map (nb_field => nb_field.text.toInt)
			special_condition(nb_fields_results) match {
				case "OK" => {
					// Le formulaire a été correctement remplis, on "supprime la fenetre du formulaire et 
					// la fonction qui a appelée le formulaire peut en récupérer les résultats dans l'IndexedSeq "nb_fields_results"
					form_accepted = true
					this_form.visible = false
				}
				case error_message => {
					//Les réponses aux champs numériques du formulaire ne satisfont pas la special_condition
					//On remet les valeurs de tout les champs numériques à leurs valeurs par défaut et
					//on affiche une fenetre contenant le message d'erreur renvoyé par special_condition.
					//Cette fenetre disparait quand on clique sur le bouton
					for (i <- 0 until nb_fields_list.length) {
						nb_fields_def_list(i) match {
							case (field_name, inf_bound, sup_bound) =>
								nb_fields_list(i).text = ((inf_bound + sup_bound)/2).toString
							}
					}
					var error_message_window = new Dialog {
						modal = true
						val this_dialog = this
						title = "Formulaire mal remplis"
						def close_error_window ={
							this_dialog.visible = false
						}
						contents = new Button(error_message) {
							action = Action(error_message)(close_error_window)
						}
					}
					error_message_window.visible = true
				}
			}	
			//Construction de comboboxes_results (IndexedSeq contenant les résultats des comboboxes)
			if (!no_comboboxes) {
				comboboxes_results = comboboxes_list map (combobox => combobox.item)
			}
		}
	}
	contents = form_panel
	centerOnScreen()
	visible = true
}
}	//Accolade fermante du package GUI