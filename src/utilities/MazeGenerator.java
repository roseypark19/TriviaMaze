package utilities;

import java.awt.Point;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;

import model.MazeTile;
import model.Movement;

public class MazeGenerator {
	
	private static final int MIN_TILES = 90;
	private static final int MAX_TILES = 110;
	private static final int SIZE = 15;
	private static final Point START = new Point(24, 72);
	private static final Point FINISH = new Point(696, 744);
	private static final Point ENTRY = new Point(24, 24);
	private static final Point EXIT = new Point(696, 792);
	
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
	
	public static Point getEntryPoint() {
		return new Point(ENTRY);
	}
	
	public static Map<Point, MazeTile> getMaze() {
		Map<Point, MazeTile> maze = new HashMap<>();
		final Deque<Point> pointStack = new ArrayDeque<>();
		final List<Movement> moves = new LinkedList<>();
		for (final Movement move : Movement.values()) {
			moves.add(move);
		}
		maze.put(ENTRY, new MazeTile(ENTRY));
		Point curr = START;
		while (curr.getX() != FINISH.getX() || curr.getY() != FINISH.getY()) {
			maze.put(curr, new MazeTile(curr));
			pointStack.push(curr);
			Collections.shuffle(moves);
			boolean moveComplete = false;
			int tries = 0;
			do {
				final Movement nextMove = moves.remove(0);
				final Point tryPoint = maze.get(curr).getPointForMovement(nextMove);
				if (!maze.containsKey(tryPoint) && isValidPoint(tryPoint)) {
					curr = tryPoint;
					moveComplete = true;
				} else {
					tries++;
				}
				moves.add(nextMove);
			} while (!moveComplete && tries < moves.size());
			if (!moveComplete) {
				pointStack.pop();
				curr = pointStack.pop();
			}
		}
		maze.put(curr, new MazeTile(curr));
		maze.put(EXIT, new MazeTile(EXIT));
		if (maze.size() < MIN_TILES || maze.size() > MAX_TILES) {
			maze = getMaze();
		}
		return maze;
	}
	
	private static Set<GraphEdge> getFinalizedEdges() {
		return kruskalsAlgorithm(getEdges());
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
	
	private static Set<GraphEdge> kruskalsAlgorithm(final List<GraphEdge> theEdges) {
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
			boolean isCycle = false;
			if (firstGroup != secondGroup) { // does not create a cycle
				finder.unify(firstGroup, secondGroup);
			} else {
				isCycle = true;
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
		
	private static boolean isValidPoint(final Point thePoint) {
		return thePoint.getX() >= START.getX() && thePoint.getY() >= START.getY() &&
			   thePoint.getX() <= FINISH.getX() && thePoint.getY() <= FINISH.getY();
	}
	
	public static Map<Point, MazeTile> getNewMaze() {
		final Map<Point, MazeTile> maze = new HashMap<>();
		final List<MazeTile> tiles = getTiles();
		final Set<GraphEdge> chosenEdges = getFinalizedEdges();
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
	
	
}
