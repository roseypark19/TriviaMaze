package controller;

import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

import model.Movement;
import view.MazePanel;

public class KeyboardHandler extends KeyAdapter {

	@Override
	public void keyTyped(final KeyEvent theEvent) {
		MazePanel.getInstance().initializeAdvancement(
				          Movement.valueof(Character.toUpperCase(theEvent.getKeyChar())));
	}

}
