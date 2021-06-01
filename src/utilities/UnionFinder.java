/*
 * UnionFinder.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package utilities;

/**
 * UnionFinder is a class for maintaining groups of connected edges in a graph. In doing so,
 * a UnionFinder determines whether adding a new edge to a group will produce a cycle in the
 * resulting graph
 * 
 * Adapted from https://www.youtube.com/watch?v=KbFlZYCpONw&t=175s
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 *
 */
public class UnionFinder {
	
	/** The number of elements in this UnionFinder */
	private final int mySize;
	
	/** The size of each group in this UnionFinder */
	private final int[] mySizes;
	
	/** 
	 * The group ID numbers associated with this UnionFinder.
	 * myIds[i] points to the parent of i. If myIds[i] = i, then i is a root node.
	 */
	private final int[] myIds;
	
	/** The current number of groups in this UnionFinder */
	private int myComponentCount;
	
	/**
	 * Constructs a new UnionFinder using the provided size.
	 * 
	 * @param theSize the size of the UnionFinder to be constructed
	 */
	public UnionFinder(final int theSize) {
		if (theSize < 0) {
			throw new IllegalArgumentException("Sizes must be non-negative!");
		}
		mySize = myComponentCount = theSize;
		mySizes = new int[mySize];
		myIds = new int[mySize];
		for (int index = 0; index < mySize; index++) {
			myIds[index] = index; // link to itself (self root)
			mySizes[index] = 1; // each group is originally of size one
		}
	}
	
	/**
	 * Finds which group number theP belongs to.
	 * 
	 * @param theP the ID number for a specific vertex
	 * @return the group number for which theP belongs to
	 */
	public int find(final int theP) {
		int p = theP;
		int root = p;
		// find the group number
		while (root != myIds[root]) {
			root = myIds[root];
		}
		// compress the path leading back to the root
		while (p != root) {
			final int next = myIds[p];
			myIds[p] = root;
			p = next;
		}
		return root;
	}
	
	/**
	 * Indicates whether or not theP and theQ are in the same group.
	 * 
	 * @param theP the ID number of the first vertex
	 * @param theQ theID number of the second vertex
	 * @return true if theP and theQ are in the same group, false otherwise
	 */
	public boolean isConnected(final int theP, final int theQ) {
		return find(theP) == find(theQ);
	}
	
	/**
	 * Retrieves the size of the group for which theP belongs to.
	 * 
	 * @param theP the ID number of a specific vertex
	 * @return the group size
	 */
	public int getComponentSize(final int theP) {
		return mySizes[find(theP)];
	}

	/**
	 * Retrieves the number of elements in this UnionFinder.
	 * 
	 * @return the number of elements
	 */
	public int getSize() {
		return mySize;
	}
	
	/**
	 * Retrieves the number of groups in this UnionFinder.
	 * 
	 * @return the number of groups
	 */
	public int getComponentCount() {
		return myComponentCount;
	}
	
	/**
	 * Unifies the sets containing theP and theQ.
	 * 
	 * @param theP the ID number of the first vertex
	 * @param theQ the ID number of the second vertex
	 */
	public void unify(final int theP, final int theQ) {
		final int root1 = find(theP);
		final int root2 = find(theQ);	
		if (root1 != root2) { // if the elements are already in the same group, we are done
			// merge smaller group into larger group
			if (mySizes[root1] < mySizes[root2]) {
				mySizes[root2] += mySizes[root1];
				myIds[root1] = root2;
			} else {
				mySizes[root1] += mySizes[root2];
				myIds[root2] = root1;
			}
			myComponentCount--; // we now have one less group following the unification
		}
	}
}
