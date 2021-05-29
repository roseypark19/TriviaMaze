package components;

import java.io.Serializable;

import view.PlayPanel.TriviaPanel;

public interface TriviaComponent extends Serializable {
	
	void addActionListener(final TriviaPanel theTrivPan);

}
