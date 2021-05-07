package utilities;

public class UnionFinder {
	
	private int mySize;
	private int[] mySizes;
	private int[] myIds;
	private int myComponentCount;
	
	public UnionFinder(final int theSize) {
		mySize = myComponentCount = theSize;
		mySizes = new int[mySize];
		myIds = new int[mySize];
		for (int index = 0; index < mySize; index++) {
			myIds[index] = index;
			mySizes[index] = 1;
		}
	}
	
	public int find(final int theP) {
		int p = theP;
		int root = p;
		while (root != myIds[root]) {
			root = myIds[root];
		}
		
		while (p != root) {
			final int next = myIds[p];
			myIds[p] = root;
			p = next;
		}
		return root;
	}
	
	public boolean isConnected(final int theP, final int theQ) {
		return find(theP) == find(theQ);
	}
	
	public int getComponentSize(final int theP) {
		return mySizes[find(theP)];
	}

	public int getSize() {
		return mySize;
	}
	
	public int getComponentCount() {
		return myComponentCount;
	}
	
	public void unify(final int theP, final int theQ) {
		int root1 = find(theP);
		int root2 = find(theQ);
		
		if (root1 != root2) {
			if (mySizes[root1] < mySizes[root2]) {
				mySizes[root2] += mySizes[root1];
				myIds[root1] = root2;
			} else {
				mySizes[root1] += mySizes[root2];
				myIds[root2] = root1;
			}
			myComponentCount--;
		}
	}
}
