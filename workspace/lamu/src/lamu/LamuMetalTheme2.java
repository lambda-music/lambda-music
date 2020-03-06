package lamu;

import javax.swing.UIDefaults;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.LineBorder;
import javax.swing.plaf.BorderUIResource;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.basic.BasicBorders;
import javax.swing.plaf.metal.DefaultMetalTheme;

/**
 * Added as a sample for changing color. 
 */
public class LamuMetalTheme2 extends DefaultMetalTheme {
	private static final ColorUIResource primary1         = new ColorUIResource(   0,   0,   0);
	private static final ColorUIResource primary2         = new ColorUIResource( 204, 204, 204);
	private static final ColorUIResource primary3         = new ColorUIResource( 255, 255, 255);
	private static final ColorUIResource primaryHighlight = new ColorUIResource( 102, 102, 102);
	private static final ColorUIResource secondary2       = new ColorUIResource( 204, 204, 204);
	private static final ColorUIResource secondary3       = new ColorUIResource( 255, 255, 255);
	private static final ColorUIResource controlHighlight = new ColorUIResource( 102, 102, 102);


	// This does not override getSecondary1 (102,102,102)

	public String getName() {
		return "Contrast";
	}

	protected ColorUIResource getPrimary1() {
		return primary1;
	}

	protected ColorUIResource getPrimary2() {
		return primary2;
	}

	protected ColorUIResource getPrimary3() {
		return primary3;
	}
	
	public ColorUIResource getPrimaryControlHighlight() {
		return primaryHighlight;
	}

	protected ColorUIResource getSecondary2() {
		return secondary2;
	}

	protected ColorUIResource getSecondary3() {
		return secondary3;
	}
	
	

	public ColorUIResource getControlHighlight() {
		// This was super.getSecondary3();
		return secondary2;
	}

	public ColorUIResource getFocusColor() {
		return getBlack();
	}

	public ColorUIResource getTextHighlightColor() {
		return getBlack();
	}

	public ColorUIResource getHighlightedTextColor() {
		return getWhite();
	}

	public ColorUIResource getMenuSelectedBackground() {
		return getBlack();
	}

	public ColorUIResource getMenuSelectedForeground() {
		return getWhite();
	}

	public ColorUIResource getAcceleratorForeground() {
		return getBlack();
	}

	public ColorUIResource getAcceleratorSelectedForeground() {
		return getWhite();
	}

	public void addCustomEntriesToTable(UIDefaults table) {
		Border blackLineBorder = new BorderUIResource(new LineBorder(
				getBlack()));
		Border whiteLineBorder = new BorderUIResource(new LineBorder(
				getWhite()));
		Object textBorder = new BorderUIResource(new CompoundBorder(
				blackLineBorder, new BasicBorders.MarginBorder()));

		Object[] defaults = new Object[] {
				"ToolTip.border", blackLineBorder,

				"TitledBorder.border", blackLineBorder,

				"TextField.border", textBorder,

				"PasswordField.border", textBorder,

				"TextArea.border", textBorder,

				"TextPane.border", textBorder,

				"EditorPane.border", textBorder,

				"ComboBox.background", getWindowBackground(),
				"ComboBox.foreground", getUserTextColor(),
				"ComboBox.selectionBackground", getTextHighlightColor(),
				"ComboBox.selectionForeground", getHighlightedTextColor(),

				"ProgressBar.foreground",  getUserTextColor(),
				"ProgressBar.background", getWindowBackground(),
				"ProgressBar.selectionForeground", getWindowBackground(),
				"ProgressBar.selectionBackground", getUserTextColor(),

				"OptionPane.errorDialog.border.background",
				getPrimary1(),
				"OptionPane.errorDialog.titlePane.foreground",
				getPrimary3(),
				"OptionPane.errorDialog.titlePane.background",
				getPrimary1(),
				"OptionPane.errorDialog.titlePane.shadow",
				getPrimary2(),
				"OptionPane.questionDialog.border.background",
				getPrimary1(),
				"OptionPane.questionDialog.titlePane.foreground",
				getPrimary3(),
				"OptionPane.questionDialog.titlePane.background",
				getPrimary1(),
				"OptionPane.questionDialog.titlePane.shadow",
				getPrimary2(),
				"OptionPane.warningDialog.border.background",
				getPrimary1(),
				"OptionPane.warningDialog.titlePane.foreground",
				getPrimary3(),
				"OptionPane.warningDialog.titlePane.background",
				getPrimary1(),
				"OptionPane.warningDialog.titlePane.shadow",
				getPrimary2(),
		};

		table.putDefaults(defaults);
	}
}
