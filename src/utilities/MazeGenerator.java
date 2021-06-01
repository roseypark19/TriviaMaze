/*
 * MazeGenerator.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package utilities;

import java.awt.Point;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Random;
import java.util.Set;
import model.MazeTile;

/**
 * MazeGenerator is a class for generating a random maze tile layout. This is achieved by
 * creating a randomly assigned point-based mapping to each maze tile.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class MazeGenerator {

	/** The number of rows and columns in each maze */
	public static final int SIZE = 17;
	
	/** The default maze start point - this is where each randomized layout begins. */
	private static final Point START = new Point(72, 72);
	
	/** The default maze entry point */
	private static final Point ENTRY = new Point(72, 24);
	
	/** The default maze exit point */
	private static final Point EXIT = new Point(72 + (SIZE - 1) * MazeTile.SIZE, 
			                                    24 + (SIZE + 1) * MazeTile.SIZE);
	
	/**
	 * Creates and supplies a random point-based mapping to individual maze tiles. 
	 * 
	 * @return the point-based mapping to each maze tile
	 */
	public static Map<Point, MazeTile> generateTileMap() {
		final Map<Point, MazeTile> maze = new HashMap<>();
		final List<MazeTile> tiles = getTiles();
		final Set<GraphEdge> chosenEdges = kruskalMST(getEdges());
		for (final GraphEdge edge : chosenEdges) {
			final MazeTile first = tiles.get(edge.getFirst());
			final MazeTile last = tiles.get(edge.getSecond());
			int shiftFactor = 1; // assumes tiles are in the same row
			if (first.getID() / SIZE != last.getID() / SIZE) { // tiles are in same column
				shiftFactor = SIZE;
			} 
			for (int id = first.getID(); id <= last.getID(); id += shiftFactor) {
				final MazeTile toPut = tiles.get(id);
				maze.put(toPut.getPoint(), toPut);
			}
		}
		maze.put(ENTRY, new MazeTile(ENTRY));
		maze.put(EXIT, new MazeTile(EXIT));
		return maze;
	}

	/**
	 * Provides a copy of the default start point.
	 * 
	 * @return the default start point
	 */
	public static Point getStartPoint() {
		return new Point(START);
	}
	
	/**
	 * Provides a copy of the default entry point.
	 * 
	 * @return the default entry point
	 */
	public static Point getEntryPoint() {
		return new Point(ENTRY);
	}
	
	/**
	 * Provides a copy of the default exit point.
	 * 
	 * @return the default exit point
	 */
	public static Point getExitPoint() {
		return new Point(EXIT);
	}
	
	/**
	 * Returns a list with SIZE x SIZE maze tiles to be considered for a new point-based
	 * mapping. Note that each maze tile in this list will have a unique ID number.
	 * 
	 * @return the list of maze tiles
	 */
	private static List<MazeTile> getTiles() {
		final List<MazeTile> tiles = new ArrayList<>();
		final int startX = (int) START.getX();
		final int startY = (int) START.getY();
		int currX = startX;
		int currY = startY;
		for (int row = 0; row < SIZE; row++) {
			for (int col = 0; col < SIZE; col++) {
				tiles.add(new MazeTile(new Point(currX, currY)));
				currX += MazeTile.SIZE;
			}
			currX = startX;
			currY += MazeTile.SIZE;
		}
		return tiles;
	}
	
	/**
	 * Creates and supplies a list of graph edges connecting specific maze tiles. Edges are
	 * added for all tiles in even rows and columns, and each maze tile vertex will have
	 * an edge between itself and its neighbor two rows below and two columns to the right.
	 * This is to ensure that no rows/columns of tiles are adjacent to one another. Note that
	 * all edges in the returned list of edges have a randomly assigned cost.
	 * 
	 * @return the list of graph edges
	 */
	private static List<GraphEdge> getEdges() {
		final List<GraphEdge> edges = new ArrayList<>();
		final Random rand = new Random();
		final int vertsPerRow = SIZE / 2 + 1;
		int id = 0;
		while (id < SIZE * SIZE) {
			final int rowStart = id;
			final int maxRowVal = id + SIZE - 1;
			for (int count = 1; count <= vertsPerRow; count++) {
				if (id + 2 <= maxRowVal) {
					edges.add(new GraphEdge(id, id + 2, rand.nextInt(SIZE) + 1));
				}
				if (id + 2 * SIZE < SIZE * SIZE) {
					edges.add(new GraphEdge(id, id + 2 * SIZE, rand.nextInt(SIZE) + 1));
				}
				id += 2;
			}
			id = rowStart + 2 * SIZE;
		}
		return edges;
	}
	
	/**
	 * Creates and supplies a minimum spanning tree for a provided list of graph edges using
	 * Kruskal's algorithm. 
	 * 
	 * @param theEdges the list of graph edges
	 * @return a set of graph edges which together form the minimum spanning tree
	 */
	private static Set<GraphEdge> kruskalMST(final List<GraphEdge> theEdges) {
		final Queue<GraphEdge> edgeQueue = new PriorityQueue<>((theFirst, theSecond) -> {
			return theFirst.getCost() - theSecond.getCost();
		});
		edgeQueue.addAll(theEdges);
		final Map<Integer, Integer> vertexMapping = createVertexMap(theEdges);
		final UnionFinder finder = new UnionFinder(vertexMapping.size());
		final Set<GraphEdge> mst = new HashSet<>();
		while (!edgeQueue.isEmpty()) {
			final GraphEdge edge = edgeQueue.remove();
			final int first = edge.getFirst();
			final int second = edge.getSecond();
			final int firstGroup = finder.find(vertexMapping.get(first));
			final int secondGroup = finder.find(vertexMapping.get(second));
			if (firstGroup != secondGroup) { // does not create a cycle
				finder.unify(firstGroup, secondGroup);
				mst.add(edge);
			}
		}
		return mst;
	}
	
	/**
	 * Creates a unique ID number mapping for each vertex in a supplied list of graph edges.
	 * The mapping returned by this method is utilized by the UnionFinder class for detecting
	 * graph cycles.
	 * 
	 * @param theEdges the list of graph edges
	 * @return the unique ID number mapping
	 */
	private static Map<Integer, Integer> createVertexMap(final List<GraphEdge> theEdges) {
		final Map<Integer, Integer> vertMap = new HashMap<>();
		int mappingId = 0;
		for (final GraphEdge edge : theEdges) {
			if (vertMap.putIfAbsent(edge.getFirst(), mappingId) == null) {
				mappingId++;
			}
			if (vertMap.putIfAbsent(edge.getSecond(), mappingId) == null) {
				mappingId++;
			}
		}
		return vertMap;
	}	
	
}
