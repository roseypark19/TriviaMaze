package utilities;

public class GraphEdge {
	
	private final int myFirst;
	
	private final int mySecond;
	
	private final int myCost;
	
	public GraphEdge(final int theFirst, final int theSecond, final int theCost) {
		myFirst = theFirst;
		mySecond = theSecond;
		myCost = theCost;
	}
	
	public int getFirst() {
		return myFirst;
	}
	
	public int getSecond() {
		return mySecond;
	}
	
	public int getCost() {
		return myCost;
	}
}
