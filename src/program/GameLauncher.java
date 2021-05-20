package program;

import view.GameFrame;

public class GameLauncher {
	
	public GameLauncher() {
		GameFrame.getInstance();
	}
	
	public static void main(final String[] theArgs) {
		new GameLauncher();
	}
}
