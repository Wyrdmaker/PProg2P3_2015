import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat
import java.awt.event.{ActionEvent, ActionListener}

package GUI{

//Les états de label sont des sets de paramètres graphiques et contienent une fonction permettant de changer les paramètres graphiques d'un 
//	label pour qu'ils s'accordent à ceux de l'état
abstract class Label_State[Game_Label_Class <: Grid_Label] {
	val state_name: String

	//val size_x: Int
	//val size_y: Int
	val label_border: javax.swing.border.Border
	val opaque: Boolean
	val background: Color
	val foreground: Color
	val text: String
	var changing_text_enabled: Boolean = true //Si true, un changement d'état vers cet état ne changera pas le text du label
	var custom_painting: ((Graphics2D,Label) => Unit) = ((g:Graphics2D, l:UIElement) => ())
	//var icon: javax.swing.Icon = null

	def change_to_state(game_label: Game_Label_Class) = {
		game_label.state = state_name
		//game_label.preferredSize = new Dimension(size_x,size_y)
		game_label.border = label_border
		game_label.opaque = opaque
		game_label.background = background
		game_label.foreground = foreground
		if (changing_text_enabled)
			game_label.text = text
		game_label.custom_painting = custom_painting
		//game_label.icon = icon
		game_label.repaint()
	}
}

}	//Accolade fermante du package GUI
