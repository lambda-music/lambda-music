package lamu.lib;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Spliterator;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;

public class NullList<E> implements List<E> {
    public static final List NULL_LIST = new NullList<>();

    private final List<E> list = Collections.EMPTY_LIST;
    public int size() {
        return list.size();
    }

    public boolean isEmpty() {
        return list.isEmpty();
    }

    public boolean contains(Object o) {
        return list.contains( o );
    }

    public Iterator<E> iterator() {
        return list.iterator();
    }

    public Object[] toArray() {
        return list.toArray();
    }

    public <T> T[] toArray(T[] a) {
        return list.toArray( a );
    }

    public boolean add(E e) {
//        return list.add( e );
        return true;
    }

    public boolean remove(Object o) {
        return list.remove( o );
    }

    public boolean containsAll(Collection<?> c) {
        return list.containsAll( c );
    }

    public boolean addAll(Collection<? extends E> c) {
        return true;
//        return list.addAll( c );
    }

    public boolean addAll(int index, Collection<? extends E> c) {
        return true;
//        return list.addAll( index, c );
    }

    public boolean removeAll(Collection<?> c) {
        return list.removeAll( c );
    }

    public boolean retainAll(Collection<?> c) {
        return list.retainAll( c );
    }

    public void replaceAll(UnaryOperator<E> operator) {
        list.replaceAll( operator );
    }

    public boolean removeIf(Predicate<? super E> filter) {
        return list.removeIf( filter );
    }

    public void sort(Comparator<? super E> c) {
        list.sort( c );
    }

    public void clear() {
        list.clear();
    }

    public boolean equals(Object o) {
        return list.equals( o );
    }

    public int hashCode() {
        return list.hashCode();
    }

    public E get(int index) {
        return list.get( index );
    }

    public E set(int index, E element) {
        return list.set( index, element );
    }

    public void add(int index, E element) {
//        list.add( index, element );
    }

    public Stream<E> stream() {
        return list.stream();
    }

    public E remove(int index) {
        return list.remove( index );
    }

    public Stream<E> parallelStream() {
        return list.parallelStream();
    }

    public int indexOf(Object o) {
        return list.indexOf( o );
    }

    public int lastIndexOf(Object o) {
        return list.lastIndexOf( o );
    }

    public ListIterator<E> listIterator() {
        return list.listIterator();
    }

    public ListIterator<E> listIterator(int index) {
        return list.listIterator( index );
    }

    public List<E> subList(int fromIndex, int toIndex) {
        return list.subList( fromIndex, toIndex );
    }

    public Spliterator<E> spliterator() {
        return list.spliterator();
    }
    
    
    
}
