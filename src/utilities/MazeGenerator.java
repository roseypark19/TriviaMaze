package utilities;

import java.awt.Point;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Random;
import java.util.Set;
import model.MazeTile;
import model.Tavern;

public class MazeGenerator {

	private static final int SIZE = 17;
	private static final Point START = new Point(72, 72);
	private static final Point ENTRY = new Point(72, 24);
	private static final Point EXIT = new Point(72 + (SIZE - 1) * MazeTile.SIZE, 
			                                    24 + (SIZE + 1) * MazeTile.SIZE);
	private static final Point FLAG_POINT = new Point((int) EXIT.getX() - MazeTile.SIZE,
													  (int) EXIT.getY());
	
	public static Map<Point, MazeTile> getNewMaze() {
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
	
	public static Map<Point, Tavern> getTaverns() {
		final Map<Point, Tavern> tavernMap = new HashMap<>();
		final List<Point> points = new LinkedList<>();
		points.add(START);
		for (int count = 1; count <= SIZE / 2; count++) {
			final Point prevPt = points.get(points.size() - 1);
			points.add(new Point((int) prevPt.getX() + 2 * Tavern.SIZE, 
					                                                (int) prevPt.getY()));
		}
		final Random rand = new Random();
		for (int row = 0; row < SIZE; row += 2) {
			final List<Point> randPts = new ArrayList<>();
			for (int count = 1; count <= 3; count++) {
				randPts.add(points.remove(rand.nextInt(points.size())));
			}
			for (final Point pt : randPts) {
				final Point newPoint = new Point(pt);
				tavernMap.put(newPoint, new Tavern(newPoint));
				points.add(pt);
			}
			for (final Point pt : points) {
				pt.setLocation(pt.getX(), pt.getY() + 2 * Tavern.SIZE);
			}
		}
		return tavernMap;
	}
	
	public static Point getEntryPoint() {
		return new Point(ENTRY);
	}
	
	public static Point getExitPoint() {
		return new Point(EXIT);
	}
	
	public static Point getFlagPoint() {
		return new Point(FLAG_POINT);
	}
	
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
			boolean isCycle = true;
			if (firstGroup != secondGroup) { // does not create a cycle
				finder.unify(firstGroup, secondGroup);
				isCycle = false;
			}
			if (!isCycle) {
				mst.add(edge);
			}
		}
		return mst;
	}
	
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
