package utilities;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import org.sqlite.SQLiteDataSource;

import model.Trivia;
import model.TriviaFactory;
import model.TriviaType;

public class TriviaUtilities {

	final private static List<Trivia> TRIVIA_LIST = new LinkedList<>();

	final private static TriviaFactory FACTORY = new TriviaFactory();

	public static List<Trivia> getTriviaList() {
		if (TRIVIA_LIST.isEmpty()) {
			SQLiteDataSource dataSource = accessDataSource();
			addTriviaToList(dataSource);
		}
		final List<Trivia> trivCopy = getTriviaListCopy();
		Collections.shuffle(trivCopy);
		return trivCopy;
	}

	private static List<Trivia> getTriviaListCopy() {
		final List<Trivia> copy = new LinkedList<>();
		for (final Trivia triv : TRIVIA_LIST) {
			copy.add(triv.copy());
		}
		return copy;
	}

	/**
	 * Getting trivia from database and adding it into list.
	 * 
	 * @param theDataSource
	 * @param theTriviaType
	 */
	private static void addTriviaToList(final SQLiteDataSource theDataSource) {
		ResultSet rs = null;
		try (Connection conn = theDataSource.getConnection(); Statement stmt = conn.createStatement();) {
			final String query = "SELECT * FROM ";
			rs = stmt.executeQuery(query + TriviaType.MULTICHOICE.toString());
			addMultiChoiceToList(rs);
			rs = stmt.executeQuery(query + TriviaType.TRUEFALSE.toString());
			addTrueFalseToList(rs);
			rs = stmt.executeQuery(query + TriviaType.SHORTANSWER.toString());
			addShortAnswerToList(rs);
		} catch (SQLException e) {
			e.printStackTrace();
			System.exit(0);
		}
	}

	/**
	 * Add short answer trivia from database into list.
	 * 
	 * @param theSet
	 * @throws SQLException
	 */
	private static void addShortAnswerToList(final ResultSet theSet) throws SQLException {
		final String queryQuestion = "question";
		final String queryCorrect = "correct";
		while (theSet.next()) {
			final String question = theSet.getString(queryQuestion);
			final String correct = theSet.getString(queryCorrect);
			TRIVIA_LIST.add(FACTORY.createTrivia(correct, question, TriviaType.SHORTANSWER, null));
		}
	}

	/**
	 * Add multiple choice trivia from database into list.
	 * 
	 * @param theSet
	 * @throws SQLException
	 */
	private static void addMultiChoiceToList(final ResultSet theSet) throws SQLException {
		final String queryQuestion = "question";
		final String queryCorrectLetter = "correctletter";
		final String queryAnswers = "answers";
		while (theSet.next()) {
			final String question = theSet.getString(queryQuestion);
			final String correct = theSet.getString(queryCorrectLetter);
			final String answers = theSet.getString(queryAnswers);
			final String[] parsedAnswers = parseMultiChoiceAnswers(answers);
			TRIVIA_LIST.add(FACTORY.createTrivia(correct, question, TriviaType.MULTICHOICE, parsedAnswers));
		}
	}

	/**
	 * Add true false trivia from database into list.
	 * 
	 * @param theSet
	 * @throws SQLException
	 */
	private static void addTrueFalseToList(final ResultSet theSet) throws SQLException {
		final String queryQuestion = "question";
		final String queryCorrect = "correctbool";
		while (theSet.next()) {
			final String question = theSet.getString(queryQuestion);
			final String correct = theSet.getString(queryCorrect);
			TRIVIA_LIST.add(FACTORY.createTrivia(correct, question, TriviaType.TRUEFALSE, null));
		}
	}

	/**
	 * Helper for parsing string query from database.
	 * 
	 * @param theQueryString
	 * @return
	 */
	private static String[] parseMultiChoiceAnswers(final String theQueryString) {
		String[] answers = theQueryString.split(",");
		for (int i = 0; i < answers.length; i++) {
			answers[i] = answers[i].trim();
		}
		return answers;
	}

	/**
	 * 
	 * @return SQLiteDataSource for accessing database.
	 */
	private static SQLiteDataSource accessDataSource() {
		SQLiteDataSource dataSource = null;
		try {
			dataSource = new SQLiteDataSource();
			dataSource.setUrl("jdbc:sqlite:trivia.db");
		} catch (Exception e) {
			e.printStackTrace();
			System.exit(0);
		}
		return dataSource;
	}

}
