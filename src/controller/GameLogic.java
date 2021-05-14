package controller;

public class GameLogic {
	/* Player health in MazePanel */
	/* Player position MazePanel */
	
	public static GameLogic uniqueInstance = new GameLogic();
	public int myGameStatus;
	public static final int GAME_WIN = 2;
	public static final int GAME_PLAYING = 1;
	public static final int GAME_LOSS = 0;
	
	private GameLogic() {
		myGameStatus = GAME_PLAYING;
	}
	
	public void updateGame(final int gameStatus) {
		if (gameStatus==0) myGameStatus = GAME_LOSS;
		else if (gameStatus==2) myGameStatus = GAME_WIN;
		else myGameStatus = 1;
	}
	
}
