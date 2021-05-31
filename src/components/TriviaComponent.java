/*
 * TriviaComponent.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package components;

import java.io.Serializable;

import view.PlayPanel.TriviaPanel;

/**
 * TriviaComponent is an interface implemented by all play panel components which supply
 * responses to displayed trivia questions.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public interface TriviaComponent extends Serializable {
	
	/**
	 * Adds an action listener to this component which sends the choice value of this
	 * component to the provided parent trivia panel for processing. 
	 * 
	 * @param theTrivPan the parent trivia panel of this component
	 */
	void addActionListener(final TriviaPanel theTrivPan);

}
