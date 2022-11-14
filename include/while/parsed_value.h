#pragma once

template<typename T>
class ParsedValue {
public:
	ParsedValue(const ParsedValue&) = delete;

	ParsedValue(T&& o) : data(new T(std::move(o))){
	}

	template<typename TT>
	ParsedValue(ParsedValue<TT>&& o) {
		data = dynamic_cast<T*>(o.release());
	}
	ParsedValue(T* d) {
		data = d;
	}

	static ParsedValue error() {
		return ParsedValue();
	}
	operator bool() const {
		return data != nullptr;
	}

	bool has_value()const {
		return data != nullptr;
	}

	T* operator -> () {
		return data;
	}

	T& operator * () {
		return *data;
	}

	T* release() {
		auto tmp = data;
		data = nullptr;
		return tmp;
	}
	template<typename TT>
	ParsedValue& operator = (ParsedValue<TT>&& o) {
		if (data != nullptr) {
			delete data;
			data = nullptr;
		}
		data = dynamic_cast<T*>(o.release());
		return *this;
	}


protected:
	T* data;
	ParsedValue() {
		data = nullptr;
	}
};