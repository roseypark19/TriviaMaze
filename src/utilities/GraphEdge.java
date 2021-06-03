/*
 * GraphEdge.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package utilities;

/**
 * GraphEdge is a class which represents and stores data for individual graph edges.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class GraphEdge {
	
	/** The ID number of the first vertex */
	private final int myFirst;
	
	/** The ID number of the second vertex */
	private final int mySecond;
	
	/** The cost/weight of this vertex */
	private final int myCost;
	
	/**
	 * Constructs a new GraphEdge with the provided first and second ID numbers and cost.
	 * 
	 * @param theFirst the first vertex ID number
	 * @param theSecond the second vertex ID number
	 * @param theCost the edge cost
	 */
	GraphEdge(final int theFirst, final int theSecond, final int theCost) {
		myFirst = theFirst;
		mySecond = theSecond;
		myCost = theCost;
	}
	
	/**
	 * Provides the ID number of this edge's first vertex.
	 * 
	 * @return the ID number
	 */
	int getFirst() {
		return myFirst;
	}
	
	/**
	 * Provides the ID number of this edge's second vertex.
	 * 
	 * @return the ID number
	 */
	int getSecond() {
		return mySecond;
	}
	
	/**
	 * Provides the cost/weight of this edge.
	 * 
	 * @return the cost
	 */
	int getCost() {
		return myCost;
	}
}
