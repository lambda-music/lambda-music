package ats.metro;

import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Spliterator;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;

class MetroLogicList implements List<MetroLogic> {
	private static final String MSG_NOT_IMPLEMENTED_0 = "Not implemented. Use metro.putLogic() method";
	private static final String MSG_NOT_IMPLEMENTED = "not implemented.";
	final List<MetroTrack> object;
	public MetroLogicList(List<MetroTrack> object) {
		super();
		this.object = object;
	}

	public void forEach(Consumer<? super MetroLogic> action) {
		object.forEach((e)->{
			action.accept( e.logic );
		});
	}

	public int size() {
		return object.size();
	}

	public boolean isEmpty() {
		return object.isEmpty();
	}

	public boolean contains(Object o) {
		for ( MetroTrack track : object  ) {
			if ( o.equals( track.logic ) ) {
				return true;
			}
		}
		return false;
	}

	public Iterator<MetroLogic> iterator() {
		Iterator<MetroTrack> i = object.iterator();
		return new Iterator<MetroLogic>() {
			@Override
			public MetroLogic next() {
				return i.next().logic;
			}
			
			@Override
			public boolean hasNext() {
				return i.hasNext();
			}
		};
	}

	public Object[] toArray() {
		Object[] result = new Object[ object.size() ];
		for ( int i=0; i<object.size(); i++  ) {
			result[i] = object.get(i).logic;
		}
		return result;
	}

	public <T> T[] toArray(T[] a) {
		for ( int i=0; i<object.size(); i++  ) {
			a[i] = (T)object.get(i).logic;
		}
		return a;

	}

	public boolean add(MetroLogic e) {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED_0);
	}

	public boolean remove(Object o) {
		return object.remove(o);
	}

	public boolean containsAll(Collection<?> c) {
		for ( Object o : c  ) {
			if ( ! this.contains(o) ) {
				return false;
			}
		}
		return true;
	}

	public boolean addAll(Collection<? extends MetroLogic> c) {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED_0);
	}

	public boolean addAll(int index, Collection<? extends MetroLogic> c) {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED_0);
	}

	public boolean removeAll(Collection<?> c) {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED_0);
	}

	public boolean retainAll(Collection<?> c) {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED_0);
	}

	public void replaceAll(UnaryOperator<MetroLogic> operator) {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED_0);
	}

	public boolean removeIf(Predicate<? super MetroLogic> filter) {
		return object.removeIf( (e)->filter.test(e.logic) );
	}

	public void sort(Comparator<? super MetroLogic> c) {
		object.sort( new Comparator<MetroTrack>() {
			@Override
			public int compare(MetroTrack o1, MetroTrack o2) {
				return c.compare( o1.logic , o2.logic );
			}
		});
	}

	public void clear() {
		object.clear();
	}

	public boolean equals(Object o) {
		return object.equals(o);
	}

	public int hashCode() {
		return object.hashCode();
	}

	public MetroLogic get(int index) {
		return object.get(index).logic;
	}

	public MetroLogic set(int index, MetroLogic element) {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED_0);
	}

	public void add(int index, MetroLogic element) {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED_0);
	}

	public Stream<MetroLogic> stream() {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED );
	}

	public MetroLogic remove(int index) {
		return object.remove(index).logic;
	}

	public Stream<MetroLogic> parallelStream() {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED );
	}

	public int indexOf(Object o) {
		return object.indexOf( ((MetroLogic)o).player );
	}

	public int lastIndexOf(Object o) {
		return object.lastIndexOf( ((MetroLogic)o).player );
	}

	public ListIterator<MetroLogic> listIterator() {
		return new MetroLogicListIterator( object.listIterator() );
	}

	public ListIterator<MetroLogic> listIterator(int index) {
		return new MetroLogicListIterator( object.listIterator( index ) );
	}

	public List<MetroLogic> subList(int fromIndex, int toIndex) {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED );
	}

	public Spliterator<MetroLogic> spliterator() {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED );
	}

	
	private final class MetroLogicListIterator implements ListIterator<MetroLogic> {
		private final ListIterator<MetroTrack> listIterator;
		private MetroLogicListIterator(ListIterator<MetroTrack> listIterator) {
			this.listIterator = listIterator;
		}
		@Override
		public void set(MetroLogic e) {
			throw new RuntimeException( MSG_NOT_IMPLEMENTED );
		}

		@Override
		public void remove() {
			listIterator.remove();
		}

		@Override
		public int previousIndex() {
			return listIterator.previousIndex();
		}

		@Override
		public MetroLogic previous() {
			return listIterator.previous().logic;
		}

		@Override
		public int nextIndex() {
			return listIterator.nextIndex();
		}

		@Override
		public MetroLogic next() {
			return listIterator.next().logic;
		}

		@Override
		public boolean hasPrevious() {
			return listIterator.hasPrevious();
		}

		@Override
		public boolean hasNext() {
			return listIterator.hasNext();
		}

		@Override
		public void add(MetroLogic e) {
			throw new RuntimeException( MSG_NOT_IMPLEMENTED );
		}
	}
}