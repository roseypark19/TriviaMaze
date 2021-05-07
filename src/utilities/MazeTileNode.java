package utilities;

import java.util.Map;
import model.MazeTile;

public class MazeTileNode {
	
	private final  MazeTile myTile;
	private final Map<MazeTile, Integer> myAdjacents;
	
	public MazeTileNode(final MazeTile theTile, final Map<MazeTile, Integer> theAdjs) {
		myTile = theTile;
		myAdjacents = theAdjs; 
	}
	
	
}
