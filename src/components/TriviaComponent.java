/*
 * TriviaComponent.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package components;

import java.io.Serializable;

import view.PlayPanel.TriviaPanel;

/**
 * TriviaComponent is an interface defining behaviors to be shared by all implementing
 * subclasses.
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
	 * @throws NullPointerException if theTrivPan is null
	 */
	void addActionListener(final TriviaPanel theTrivPan);

}
