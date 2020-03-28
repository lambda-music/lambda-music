Kawapad Configuration Files
===========================

Kawapad has two configuration files :

1. `/YOUR-HOME-DIRECTORY/.kawapad/kawapad-initialization.scm`
2. `/YOUR-HOME-DIRECTORY/.kawapad/kawapad-extension.scm`

These files are Scheme file and they are executed on start-up.

1. The file `kawapad-initialization.scm` is executed _before_ any Lamu 
   components are instantiated; hence, all of Kawapad/Lamu API procedures are 
   not available in this configuration file. This configuration is for 
   initialization which can be done by calling the Java methods directly.

2. The file `kawapad-extension.scm` is executed _after_ Kawapad API is 
   initialized. At this point, Kawapad itself is not instantiated yet. This 
   configuration file is for registering event handlers of Kawapad.


# A Typical Setting for `kawapad-initialization.scm` #
A typical task for `kawapad-initialization.scm` may be initializing LookAndFeel 
for Kawapad. The following is an example of registering 

```scheme
(begin
  (define-simple-class DarkMetalTheme  
  (javax.swing.plaf.metal.DefaultMetalTheme)
                           (color-primary1   (javax.swing.plaf.ColorUIResource 1/16 1/16 1/16 ))
                           (color-primary2   (javax.swing.plaf.ColorUIResource 1/16 1/16 1/16 ))
                           (color-primary3   (javax.swing.plaf.ColorUIResource 1/16 1/16 1/16 ))
                           (color-secondary1 (javax.swing.plaf.ColorUIResource 1/16 1/16 1/16 ))
                           (color-secondary2 (javax.swing.plaf.ColorUIResource 1/16 1/16 1/16 ))
                           (color-secondary3 (javax.swing.plaf.ColorUIResource 1/16 1/16 1/16 ))
                           ((getPrimary1      )::javax.swing.plaf.ColorUIResource (this):color-primary1)
                           ((getPrimary2      )::javax.swing.plaf.ColorUIResource (this):color-primary2)
                           ((getPrimary3      )::javax.swing.plaf.ColorUIResource (this):color-primary3)
                           ((getSecondary1    )::javax.swing.plaf.ColorUIResource (this):color-secondary1)
                           ((getSecondary2    )::javax.swing.plaf.ColorUIResource (this):color-secondary2)
                           ((getSecondary3    )::javax.swing.plaf.ColorUIResource (this):color-secondary3)
                           (color-white (javax.swing.plaf.ColorUIResource 2/16 2/16 2/16 ))
                           (color-black (javax.swing.plaf.ColorUIResource 0.1 12/16 0.1))
                           (color-red   (javax.swing.plaf.ColorUIResource 0.9  2/16 0.1))

                           ((getWhite) ::javax.swing.plaf.ColorUIResource (this):color-white)
                           ((getBlack) ::javax.swing.plaf.ColorUIResource (this):color-black)

                           (color-window-background (javax.swing.plaf.ColorUIResource 0/16 0/16 0/16))
                           ((getWindowBackground) ::javax.swing.plaf.ColorUIResource (this):color-window-background )
                           ((getFocusColor    )::javax.swing.plaf.ColorUIResource (this):color-red)
                           ((getWinwodTitleForeground    )::javax.swing.plaf.ColorUIResource (this):color-red)
                           ((getWinwodTitleBackground    )::javax.swing.plaf.ColorUIResource (this):color-red))

  (javax.swing.plaf.metal.MetalLookAndFeel:setCurrentTheme 
    (DarkMetalTheme))
  (javax.swing.UIManager:setLookAndFeel
    (javax.swing.plaf.metal.MetalLookAndFeel))
  
  (kawapad.KawapadSyntaxElementType:PARENTHESIS_HIGHLIGHT:setDefaultBackgroundColor
      (java.awt.Color 0.0 0.3 0.3 1.0))

  (kawapad.KawapadSyntaxElementType:KEYWORD_HIGHLIGHT:setDefaultBackgroundColor
      (java.awt.Color 0.0 0.1 0.5 1.0)))
```

This defines a sub-class of `DefaultMetalTheme` class and set an instance of 
the class as default Look-and-Feel on the Swing Component Library.

The following code sets the font for UI to `monospaced`.

```scheme
(kawapad.Kawapad:setUIFont
  (java.awt.Font "monospaced" java.awt.Font:PLAIN 12 ))
```

The following code load a font from a specific file and set it to the font for 
UI.

```scheme
(kawapad.Kawapad:setUIFont
  (kawapad.Kawapad:loadFont "/home/someone/font/some-font-file.ttf" 20 ))
```


# a typical setting for kawapad-extension.scm #

The following code sets these kawapad editor's colors to their namely colors.

```scheme
(register-event-handler 'create 'init-font
                        (lambda (kawapad)
                          (kawapad:set-foreground          java.awt.Color:green )
                          (kawapad:set-background          java.awt.Color:black )
                          (kawapad:set-selected-text-color java.awt.Color:white )
                          (kawapad:set-selection-color     java.awt.Color:blue  )
                          (kawapad:set-border 
                            (javax.swing.BorderFactory:create-line-border java.awt.Color:black 5 ))))
```

The following code sets default LAF only for the current Kawapad instance.
```scheme
(register-event-handler 'create 'init-font
                        (lambda (kawapad)
                          (javax.swing.plaf.metal.MetalLookAndFeel:setCurrentTheme
                            (javax.swing.plaf.metal.MetalHighContrastTheme))
                          (javax.swing.SwingUtilities:updateComponentTreeUI kawapad)))
```


The following code sets the default font only for the editor.

```scheme
(register-event-handler 'create 'init-font
                        (lambda (kawapad)
                           (load-font "/path/to/some-font.ttf" 20 )))
```


