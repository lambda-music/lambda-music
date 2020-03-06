package lamu;

import java.awt.Color;

import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.FontUIResource;
import javax.swing.plaf.metal.DefaultMetalTheme;

/**
 * Added as a sample for changing color. 
 */
public class LamuMetalTheme1 extends DefaultMetalTheme {
	// NEW COLOR FOR METAL LOOK AND FEEL
	
	ColorUIResource primary1 = new ColorUIResource( 32,  32, 32);
	ColorUIResource primary2 = new ColorUIResource( 32,  32, 32);
	ColorUIResource primary3 = new ColorUIResource( 32,  32, 32);
	ColorUIResource secondary1 = new ColorUIResource(32, 32, 32);
	ColorUIResource secondary2 = new ColorUIResource(32, 32, 32);
	ColorUIResource secondary3 = new ColorUIResource(32, 32, 32);
	
	protected ColorUIResource getPrimary1() {
		return primary1;
	}

	protected ColorUIResource getPrimary2() {
		return primary2;
	}

	protected ColorUIResource getPrimary3() {
		return primary3;
	}

	protected ColorUIResource getSecondary1() {
		return secondary1;
	}

	protected ColorUIResource getSecondary2() {
		return secondary2;
	}	
	protected ColorUIResource getSecondary3() {
		return secondary3;
	}
	@Override
	public FontUIResource getControlTextFont() {
		// TODO Auto-generated method stub
		return super.getControlTextFont();
	}
	ColorUIResource white = new ColorUIResource( new Color( 0,0,0 ));
	ColorUIResource black = new ColorUIResource( new Color( 128,128,128 ));
	@Override
	protected ColorUIResource getWhite() {
		return white;
	}
	@Override
	protected ColorUIResource getBlack() {
		return black;
	}
//	@Override
//	public ColorUIResource getControl() {
//		return new ColorUIResource(new Color(32,32,32));
//	}
//	@Override
//	public ColorUIResource getControlTextColor() {
//		// TODO Auto-generated method stub
//		return getWhite();
//	}
//	@Override
//	public ColorUIResource getHighlightedTextColor() {
//		return getWhite();
//			}
//	@Override
//	public ColorUIResource getPrimaryControl() {
//		return getBlack();
//	}
//	@Override
//	public ColorUIResource getPrimaryControlHighlight() {
//		return new ColorUIResource(new Color(32,32,64));
//	}
//	@Override
//	public ColorUIResource getWindowBackground() {
//		return new ColorUIResource(new Color(32,32,64));
//	}
//	@Override
//	public ColorUIResource getControlShadow() {
//		return getBlack();
//	}
//	@Override
//	public ColorUIResource getControlHighlight() {
//		return getBlack();
//	}
//	@Override
//	public ColorUIResource getAcceleratorForeground() {
//		return getBlack();
//	}
//	@Override
//	public ColorUIResource getControlDarkShadow() {
//		return super.getControlDarkShadow();
//	}@Override
//	public ColorUIResource getControlDisabled() {
//		return super.getControlDisabled();
//	}
//	@Override
//	public ColorUIResource getControlInfo() {
//		return super.getControlInfo();
//	}
//	@Override
//	public ColorUIResource getFocusColor() {
//		return super.getFocusColor();
//	}
//	@Override
//	public ColorUIResource getDesktopColor() {
//		return super.getDesktopColor();
//	}
//	@Override
//	public ColorUIResource getPrimaryControlDarkShadow() {
//		return super.getPrimaryControlDarkShadow();
//	}
//	@Override
//	public ColorUIResource getMenuForeground() {
//		return super.getMenuForeground();
////		return getBlack();
//	}
}
